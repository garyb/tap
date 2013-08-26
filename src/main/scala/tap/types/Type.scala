package tap.types

import tap.ModuleId
import tap.types.kinds.Kind
import scala.annotation.tailrec
import tap.types.inference.{TIEnv, Substitutions}
import tap.types.inference.Substitutions.{nullSubst, Subst}
import tap.types.Natives._
import tap.util.ContextOps._
import language.implicitConversions
import language.reflectiveCalls

sealed trait Type
case class Forall(tvs: List[TyVar], t: Type) extends Type
case class TAp(f: Type, a: Type) extends Type
case class TCon(id: ModuleId, k: Kind) extends Type
case class TVar(tv: TyVar) extends Type { def id: String = tv.id }
case class MetaTv(m: Meta) extends Type

sealed trait TyVar {
    def id: String
    def k: Kind
}
case class BoundTv(id: String, k: Kind) extends TyVar
case class SkolemTv(id: String, i: Int, k: Kind) extends TyVar

case class Meta(i: Int, k: Kind)

object TFun {
    def apply(x: Type, y: Type): Type = TAp(TAp(tArrow, x), y)
    def unapply(t: Type): Option[(Type, Type)] = t match {
        case TAp(TAp(f, x), y) if f == tArrow => Some((x, y))
        case _ => None
    }
}

object Type {

    /**
     * Get all the binders used in ForAlls in the type, so that when quantifying an outer for-all we can avoid these
     * inner ones.
     */
    def tyVarBndrs(t: Type): Set[TyVar] = {
        def bndrs(t: Type, acc: Set[TyVar]): Set[TyVar] = {
            case Forall(tvs, t) => bndrs(t, acc ++ tvs)
            case TAp(x, y) => bndrs(x, bndrs(y, acc))
        }
        bndrs(t, Set.empty)
    }

    /**
     * Constructs a function type.
     */
    implicit def toFn(a: Type) = new { def fn(b: Type): Type = TAp(TAp(tArrow, a), b) }

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
    def mtv(t: Type): List[Meta] = t match {
        case Forall(_, t) => mtv(t)
        case TAp(x, y) => (mtv(x) ++ mtv(y)).distinct
        case MetaTv(m) => List(m)
        case _ => List.empty
    }


}