package tap.types.classes

import IsIn._
import tap.ModuleId
import tap.ast.FilePositional
import tap.types.{TVar, Tyvar}
import tap.types.inference.{TIInternalError, TIError}
import tap.util.PrettyPrint._
import scala.Some
import tap.util.trace


object ClassEnvironments {

	type Class = (List[Tyvar], List[IsIn], List[Inst])
	case class Inst(mId: String, ps: List[IsIn], tc: IsIn) extends FilePositional
	type ClassEnv = Map[ModuleId, Class]

	val nullEnv = Map.empty: ClassEnv

	/**
	 * Adds a new class to a class environment or replaces an existing definition.
	 */
	private def modify(ce: ClassEnv, i: ModuleId, c: Class): ClassEnv = ce + (i -> c)

	/**
	 * Helper function, used when checking if an instance overlaps with an existing instance in a class.
	 */
	private def overlap(p: IsIn, q: IsIn) = mguPred(p, q) != None

	/**
	 * Adds a class to the environment, ensuring the class is not already defined and all superclasses have already
	 * been added to the environment.
	 */
	def addClass(ce: ClassEnv, tc: TypeclassDef): ClassEnv = {
		if (ce contains tc.name) throw TIError("Class " + tc.name + " is already defined", tc)
		else if (tc.vs.isEmpty) throw new Error("Class " + tc.name + " has no variables")
		else if (tc.ps exists { p => !(ce contains p.id) }) {
			throw new Error("Class " + tc.name + " expects superclasses " + (tc.ps map { _.id }).mkString(", ") + " to already be present in class environment")
		} else if (tc.ps exists { p => p.ts exists { t => !t.isInstanceOf[TVar] }}) {
			throw new Error("Class " + tc.name + " has non-variable type in super context")
		}
		else modify(ce, tc.name, (tc.vs, tc.ps, List.empty))
	}

	/**
	 * Adds an instance to the environment.
	 */
	def addInst(ce: ClassEnv, inst: Inst): ClassEnv = {
		val Inst(_, ps, p @ IsIn(i, _)) = inst
		if (!(ce contains i)) throw new TIInternalError("Class environment is missing class " + i + ", cannot add instance")
		val its = insts(ce, i)
		val qs = its collect { case Inst(_, _, q) => q }
		if (qs exists { overlap(p, _) }) throw TIError("ovelapping instances for " + prettyPrint(p) + " (" + ps.map(prettyPrint).mkString(",")  + ")", inst)
		modify(ce, i, (sig(ce, i), `super`(ce, i), inst :: its))
	}

	def checkInsts(ce: ClassEnv): ClassEnv = {
		val insts = ce.values.flatMap { case (_, _, insts) => insts }
		insts foreach { inst =>
			bySuper(ce, inst.tc) filter { p => p != inst.tc } foreach { p =>
				if (!entail(ce, inst.ps, p)) throw new Error("Couldn't deduce " + prettyPrint(p))
			}
		}
		ce
	}

	/**
	 * Extracts the type variable signature of the specified class.
	 */
	def sig(ce: ClassEnv, i: ModuleId): List[Tyvar] =
		ce.get(i) match {
			case Some((vs, ps, its)) => vs
			case None => throw TIInternalError("Cannot find variables for '" + i + "', class is missing from the current environment.")
		}

	/**
	 * Extracts a list of superclass identifiers for the specified class.
	 */
	def `super`(ce: ClassEnv, i: ModuleId): List[IsIn] =
		ce.get(i) match {
			case Some((vs, ps, its)) => ps
			case None => throw TIInternalError("Cannot find superclasses for '" + i + "', class is missing from the current environment.")
		}

	/**
	 * Extracts a list of instances for the specified class.
	 */
	def insts(ce: ClassEnv, i: ModuleId): List[Inst] =
		ce.get(i) match {
			case Some((vs, ps, its)) => its
			case None => throw TIInternalError("Cannot find instances for '" + i + "', class is missing from the current environment.")
		}
}