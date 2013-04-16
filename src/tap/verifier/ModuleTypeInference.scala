package tap.verifier

import scala.annotation.tailrec
import tap._
import tap.ast.ASTModule
import tap.ir._
import tap.types.Type._
import tap.types.classes.ClassEnvironments.{ClassEnv, Inst}
import tap.types.classes._
import tap.types.inference.Substitutions.{Subst, tv}
import tap.types.inference.TypeInference.ExprTypeMap
import tap.types.inference.{TypeInference, Substitutions}
import tap.util.{Graph, trace, GraphUtil}
import tap.verifier.defs.{ImportedDefinitions, ModuleDefinitions}
import tap.types.{Forall, Type}

class ModuleTypeInference(val modules: Seq[ASTModule], val scopes: Map[String, ImportedDefinitions], val dependencies: Map[String, Set[String]]) {

	type ModuleName = String
	type Expl = Id
	type Impl = Id
	type ImplGrps = List[Seq[Impl]]

	def apply(defs: ModuleDefinitions): (ModuleDefinitions, Subst, ExprTypeMap) = {

		val moduleIds = modules map { m => m.name }

		val mis = defs.mis filterKeys {
			case id: ModuleId => moduleIds contains id.mId
			case id: InstId => moduleIds contains id.mId
			case id => throw new Error("Bad id in defs.mis: " + id)
		}

		// ---[ member binding groups ] -------------------------------------------------------------------------------

		val bindGroups = resolveBindingGroups(defs, mis) map { case (expls, impls) =>
			(expls map { m =>
				val t = m match {
					case InstId(mId, tcId, ps, id) =>
						val tc = defs.tcs(tcId)
						val tci = (defs.tcis(tcId) find { case Inst(_, _, IsIn(_, tciPs)) =>
							tciPs forall { tcip => ps contains getTConID(tcip) }
						}) match {
							case Some(tci) => tci
							case None => throw new Error("Unable to find appropriate instance for " + tc + " with params " + ps)
						}
						makeInstanceMemberType(defs.mts(ModuleId(tc.name.mId, id)), tci)
					case qId => defs.mts(qId)
				}
				(m, t, mis(m)) },
			impls map { l => l.map { m => (m, mis(m)) }.toList })
		}

		// ---[ class environments ] ----------------------------------------------------------------------------------

		val ces = buildClassEnv(defs)

		// ---[ type inference ] --------------------------------------------------------------------------------------

		val as = (defs.dcons mapValues { dcon => Qual(Nil, dcon) }) ++ defs.mts
		val (as2, s, ets) = TypeInference.tiProgram(ces(modules(0).name), as, bindGroups)
		val infmts = as2.collect { case (id, t) if !defs.mts.contains(id) => id -> t }
		(ModuleDefinitions(defs.tcons, defs.dcons, defs.tcs, defs.tcis, defs.mts ++ infmts, defs.mis ++ mis), s, ets)
	}

	/**
	 * Creates a member type for a specific instance of a typeclass.
	 * @param sc The member's type from the typeclass definition.
	 * @param tci The typeclass instance.
	 * @return The instantiated type.
	 */
	def makeInstanceMemberType(sc: Qual[Type], tci: Inst): Qual[Type] = {
		val qt0 = TypeInference.freshInst(sc)
		val subst = (tv(qt0) zip tci.tc.ts).toMap
		val qt1 = Qual(tci.ps, Substitutions.applySubst(subst, qt0.h))
		Qual.quantify(tv(qt1), qt1)
	}

	def buildClassEnv(defs: ModuleDefinitions): Map[String, ClassEnv] = {
		(modules map { m =>

			val mId = m.name
			val mtcs = scopes(mId).tcs

			type Classes = Map[ModuleId, TypeclassDef]

			def importTC(ce: Classes, tc: TypeclassDef): Classes = {
				val supers = tc.ps.map { pred => pred.id }
				importTCs(ce + (tc.name -> tc), supers)
			}

			def importTCs(ce: Classes, tcs: Iterable[ModuleId]): Classes =
				tcs.foldLeft(ce) { case (tcs, tc) => importTC(tcs, defs.tcs(tc)) }

			val classes = importTCs(Map.empty, mtcs.values)
			val supers = classes mapValues { tc => tc.ps map { p => p.id }}
			val classOrd = Graph.tsort(classes.keys.toSeq, supers)

			val ce = classOrd.foldLeft(ClassEnvironments.nullEnv) { case (ce, tc) =>
				ClassEnvironments.addClass(ce, classes(tc))
			}

			// Find all the instances defined by or reaching the current module
			val currModuleDeps = dependencies(mId)
			val mtcis = (mtcs.valuesIterator.filter { id => defs.tcis contains id } flatMap {
				case id => defs.tcis(id).collect {
					case inst if inst.mId == mId || (currModuleDeps contains inst.mId) => inst
				}
			})

			mId -> ClassEnvironments.checkInsts(mtcis.foldLeft(ce)(ClassEnvironments.addInst))

		}).toMap
	}

	def resolveBindingGroups(verifiedDefs: ModuleDefinitions, mis: Map[Id, TapExpr]): List[(List[Expl], ImplGrps)] = {

		// Find the dependencies for each member.
		val memDeps = mis mapValues { expr => TapNodeUtil.findDependencies(expr, Set.empty).toList }

		// Make a version of the map that only contains dependencies for explicitly typed functions.
		val explDeps = (memDeps filterKeys {
			case _: InstId => true
			case mId: ModuleId => verifiedDefs.mts contains mId
			case _: LocalId => throw new Error("LocalId in dependency list for explicitly typed function")
		})

		// Make a list of all the definitions that should not be included in the implicit definition groups - this
		// includes all data constructors (as they are generated functions that have no dependencies), all members that
		// were imported from modules that have already been verified, and all members that have been explicitly typed.
		val nonDependent = verifiedDefs.dcons.keySet ++ verifiedDefs.mts.keySet ++ explDeps.keySet

		// Get the ordered strongly connected members and map that allows us to find the dependency group for members
		// declared in the current group of modules
		val (implMemOrd, lookupDeps) = GraphUtil.getSCCOrder(memDeps, nonDependent)

		// Find the implicit definition groups that each explicitly typed member depends on
		val explDepGroups = explDeps.mapValues { ds => ds map { d => lookupDeps(d) } }.toList

		// Create the binding groups for the explicitly typed members
		@tailrec def resolveGroups(xs: ImplGrps, result: (ImplGrps, List[(List[Expl], ImplGrps)])): (ImplGrps, List[(List[Expl], ImplGrps)]) = xs match {
			case Seq() => result
			case x :: xs =>
				val (expls, impls) = explDepGroups.foldLeft((List.empty[Expl], Set.empty[Seq[Impl]])) { case (result, (expl, impls)) =>
					if (impls contains x) (expl :: result._1, result._2 ++ impls)
					else result
				}
				if (expls.nonEmpty) resolveGroups(xs filterNot impls.contains, (result._1, (expls, impls.toList) :: result._2))
				else resolveGroups(xs, (result._1 :+ x, result._2))
		}
		val (remainImpls, bindGroups0) = resolveGroups(implMemOrd, (List.empty, List.empty))
		val usedExpl = bindGroups0.flatMap { case (expl, _) => expl }
		val missingExpls = explDeps.keySet -- usedExpl

		// Add any remaining definitions
		bindGroups0 ++
		(remainImpls map { impls => (List.empty[Expl], List(impls))}) ++
		(missingExpls map { expl => (List(expl), List.empty[List[Impl]]) })
	}

}
