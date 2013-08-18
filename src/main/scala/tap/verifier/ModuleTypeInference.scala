package tap.verifier

import tap._
import tap.ast.ASTModule
import tap.ir._
import tap.types.Type._
import tap.types.classes.ClassEnvironments.{ClassEnv, Inst}
import tap.types.classes._
import tap.types.inference.Substitutions.Subst
import tap.types.inference.TypeInference.ExprTypeMap
import tap.types.inference.TypeInference
import tap.util.Graph
import tap.verifier.defs.{DefinitionsLookup, ModuleDefinitions}
import tap.types.{Forall, Type}
import tap.util.PrettyPrint._

class ModuleTypeInference(val modules: Seq[ASTModule], val scopes: Map[String, DefinitionsLookup], val dependencies: Map[String, Set[String]]) {

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

        val bindGroups = resolveBindingGroups(mis, defs.mts) map { case (expls, impls) =>
            (expls map { m =>
                val t = m match {
                    case InstId(mId, tcId, ps, id) =>
                        val tc = defs.tcs(tcId)
                        val tci = defs.tcis(tcId) find { case Inst(_, _, IsIn(_, tciPs)) =>
                            tciPs forall { tcip => ps contains getTConID(tcip) }
                        } match {
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
    def makeInstanceMemberType(sc: Qual[Type], tci: Inst): Qual[Type] = sc.h match {
        case t: Forall =>
            if (sc.ps(0).id != tci.tc.id) throw new Error("Cannot instantiate type " + prettyPrint(sc) + " with class " + prettyPrint(tci))
            if (tci.tc.ts.length != t.ks.length) throw new Error("Type parameter mismatch count for " + prettyPrint(tci) + " instantiating " + prettyPrint(sc))
            TypeInference.freshInstPartial(tci.tc.ts, sc)
        case _ => throw new Error("makeInstanceMemberType called on non-Forall type " + prettyPrint(sc))
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
            val classOrd = Graph.tsort(supers)

            val ce = classOrd.foldLeft(ClassEnvironments.nullEnv) { case (ce, tc) =>
                ClassEnvironments.addClass(ce, classes(tc))
            }

            // Find all the instances defined by or reaching the current module
            val currModuleDeps = dependencies(mId)
            val mtcis = mtcs.valuesIterator.filter { id => defs.tcis contains id } flatMap {
                case id => defs.tcis(id).collect {
                    case inst if inst.mId == mId || (currModuleDeps contains inst.mId) => inst
                }
            }

            mId -> ClassEnvironments.checkInsts(mtcis.foldLeft(ce)(ClassEnvironments.addInst))

        }).toMap
    }

    def resolveBindingGroups(mis: Map[Id, TapExpr], mts: Map[Id, Qual[Type]]): List[(List[Expl], ImplGrps)] = {

        // Find the dependencies for each member implementation, excluding dependencies to members outside of `mis`
        val deps = mis mapValues { expr =>
            TapNodeUtil.findDependencies(expr, Set.empty) filter { d => mis contains d }
        }

        // Split the member implementations based on whether they were declared with an explicit type or not
        val (explDeps, implDeps) = deps.partition {
            case (k: InstId, _) => true
            case (mId: ModuleId, _) => mts contains mId
            case (_: LocalId, _) => throw new Error("LocalId in member implementations")
        }

        // Find the dependencies implicitly typed members have on other implicitly typed members
        val implImplDeps = implDeps mapValues { deps => deps filterNot { e => explDeps contains e } }

        // Find the groups of implicitly defined members that depend upon each other
        val implGrps = Graph.components(implImplDeps)

        // ADT to allow explicit typed defs and groups of implicitly typed defs to coexist in a homogeneous collection
        sealed trait DefType
        case class ExplDef(e: Id) extends DefType
        case class ImplDefs(is: List[Id]) extends DefType

        // Create a lookup for finding the explicitly typed definition or group of implicitly typed definitions
        // associated with a particular member id
        val lookup: Map[Id, DefType] =
            Graph.makeComponentLookup(implGrps).mapValues { g => ImplDefs(g) } ++
            explDeps.map { case (e, _) => e -> ExplDef(e) }

        // Find the dependencies between explicitly typed defs and groups of implicitly typed defs
        val explGrpDeps = explDeps map { case (explDef, implDeps) =>
            ExplDef(explDef) -> (implDeps collect { case d if lookup contains d => lookup(d) })
        }

        // Find the dependencies between groups of implicitly typed defs and explicitly typed defs or other groups of
        // implicitly typed defs
        val implGrpDeps = implDeps.foldLeft(Map.empty[DefType, Set[DefType]]) { case (result, (implDef, deps)) =>
            val is = lookup(implDef)
            val ds = deps map { d => lookup(d) }
            result.get(is) match {
                case Some(ds0) => result + (is -> (ds0 ++ ds))
                case None => result + (is -> ds)
            }
        }

        // Find the binding groups by using the dependencies between explicitly typed defs and groups of implicitly
        // typed defs
        val bindGroups = Graph.components(explGrpDeps ++ implGrpDeps)

        // Split each binding group into a list of explicitly typed definitions, and a list of groups of implictly
        // typed defs, and unwrap the homogenising-ADT
        bindGroups.map { bg =>
            val (expl, impl) = bg partition { _.isInstanceOf[ExplDef] }
            val es = expl collect { case ExplDef(e) => e }
            val iss = impl collect { case ImplDefs(is) => is }
            (es, iss)
        }
    }

}
