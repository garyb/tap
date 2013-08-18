package tap.types

import tap.ModuleId
import tap.types.kinds.{Kind, Kfun, Star}
import scala.annotation.tailrec
import tap.types.inference.Substitutions
import tap.types.inference.Substitutions.{nullSubst, Subst}
import tap.util.PrettyPrint._
import language.implicitConversions
import language.reflectiveCalls

sealed trait Type
case class TVar(id: String, k: Kind) extends Type
case class TCon(id: ModuleId, k: Kind) extends Type
case class TAp(f: Type, a: Type) extends Type
case class TGen(fi: Int, i: Int) extends Type
case class Forall(i: Int, ks: List[Kind], t: Type) extends Type

object Type {

    private var forallId: Int = 0
    def newForallId(): Int = {
        forallId += 1
        forallId
    }
    def lastForallId: Int = forallId

    /**
     * Constructs a function type.
     */
    implicit def toFn(a: Type) = new { def fn(b: Type): Type = TAp(TAp(tArrow, a), b) }

    val tArrow: Type  = TCon(ModuleId("Native", "->"), Kfun(Star, Kfun(Star, Star)))
    val tNumber: Type = TCon(ModuleId("Native", "Number"), Star)
    val tString: Type = TCon(ModuleId("Native", "String"), Star)
    val tBool: Type   = TCon(ModuleId("Native", "Bool"), Star)
    val tUnit: Type   = TCon(ModuleId("Native", "Unit"), Star)
    val tVar: Type    = Type.quantify(List(TVar("a", Star)), TVar("a", Star) fn TAp(TCon(ModuleId("Native", "Var"), Kfun(Star, Star)), TVar("a", Star)))._2

    /**
     * Finds the ID of a type constructor in the specified type. This should only be called when it is known the type
     * is a TCon or an application of a TCon.
     */
    def getTConID(t: Type): ModuleId = t match {
        case TCon(id, _) => id
        case TAp(t, _) => getTConID(t)
        case _ => throw new IllegalArgumentException("non-TCon or TAp in getTConID: " + t)
    }

    /**
     * Checks whether a type is a function type.
     */
    @tailrec def isFuncType(t: Type): Boolean = t match {
        case TAp(c: TCon, _) => c == tArrow
        case TAp(t, _) => isFuncType(t)
        case _ => false
    }

    /**
     * Extracts the argument and returns types from a function as a list..
     */
    def getFunctionTypes(t: Type): List[Type] = {
        def loop(t: Type): List[Type] = t match {
            case TAp(fn, arg) if fn == tArrow => List(arg)
            case TAp(fn, arg) if isFuncType(fn) => loop(fn) ++ loop(arg)
            case t => List(t)
        }
        loop(t)
    }

    /**
     * Extracts the argument types from a function type.
     */
    def getFunctionTypeArgs(t: Type): List[Type] = getFunctionTypes(t).dropRight(1)

    /**
     * Finds the arity of a function based on its type.
     */
    def getFunctionArity(t: Type): Int = getFunctionTypes(t).length - 1

    /**
     * Finds the arity of a type based on its kind.
     */
    def getTypeArity(t: Type): Int = Kind.arity(Kind.kind(t))

    /**
     * Creates a function type from a list of types. List(a, b, c) becomes (a -> b -> c).
     */
    def makeFunctionType(ts: List[Type]) = ts.reduceRight { _ fn _ }

    /**
     * Creates a function type from a list of argument types and a return type.
     */
    def makeFunctionType(argTs: List[Type], rt: Type) = argTs.foldRight(rt) { _ fn _ }

    /**
     * Finds the type variable used within a type.
     */
    def tv(t: Type): List[TVar] = t match {
        case u: TVar => List(u)
        case TAp(l, r) => (tv(l) ++ tv(r)).distinct
        case Forall(_, _, t) => tv(t)
        case t => List.empty
    }

    /**
     * Universally quantifies t using the specified type variables.
     */
    def quantify(vs: List[TVar], t: Type): (Subst, Type) = {
        val vs1 = tv(t) collect { case v if vs contains v => v }
        if (vs1.isEmpty) (nullSubst, t)
        else {
            val ks = vs1 map Kind.kind
            val fi = newForallId()
            val s = (vs1 zip (List.range(0, vs1.size) map { n => TGen(fi, n) })).toMap
            (s, Forall(fi, ks, Substitutions.applySubst(s, t)))
        }
    }

    /**
     * Instantiates a universally quantified type, replacing the quantified types with other types.
     */
    def inst(sc: Forall, ts: List[Type], t: Type): Type = t match {
        case Forall(i, ks, t) if i == sc.i => inst(sc, ts, t)
        case Forall(i, ks, t) => Forall(i, ks, inst(sc, ts, t))
        case TAp(l, r) => TAp(inst(sc, ts, l), inst(sc, ts, r))
        case TGen(id, n) if id == sc.i => ts(n)
        case t => t
    }
}