package tap.types

import tap.ModuleId
import tap.types.kinds.Kind
import scala.annotation.tailrec
import tap.types.inference.{TIEnv, Substitutions}
import tap.types.inference.Substitutions.{nullSubst, Subst}
import tap.types.Natives._
import language.implicitConversions
import language.reflectiveCalls

sealed trait Type
case class Forall(tvs: List[Tyvar], t: Type) extends Type
case class TAp(f: Type, a: Type) extends Type
case class TCon(id: ModuleId, k: Kind) extends Type
case class TVar(tv: Tyvar) extends Type { def id: String = tv.id }
case class MetaTv(i: Int, k: Kind, var ref: Option[Type]) extends Type

sealed trait Tyvar { def id: String }
case class BoundTv(id: String, k: Kind) extends Tyvar
case class SkolemTv(id: String, i: Int, k: Kind) extends Tyvar

object Type {

    /**
     * Get the MetaTvs from a type.
     */
    def metaTvs(ts: List[Type]): Set[MetaTv] = {
        def findMetaTvs(t: Type, acc: Set[MetaTv]): Set[MetaTv] = t match {
            case Forall(_, t) => findMetaTvs(t, acc)
            case TAp(x, y) => findMetaTvs(x, findMetaTvs(y, acc))
            case m: MetaTv => acc + m
            case _ => acc
        }
        ts.foldRight(Set.empty[MetaTv])(findMetaTvs)
    }

    /**
     * Get the free TyVars from a type.
     */
    def freeTyVars(ts: List[Type]): Set[Tyvar] = {
        def go(bound: Set[Tyvar], t: Type, acc: Set[Tyvar]): Set[Tyvar] = t match {
            case Forall(tvs, t) => go(bound ++ tvs, t, acc)
            case TAp(x, y) => go(bound, x, go(bound, y, acc))
            case TVar(tv) if !(bound contains tv) => acc + tv
            case _ => acc
        }
        ts.foldRight(Set.empty[Tyvar])(go(Set.empty, _, _))
    }

    /**
     * Get all the binders used in ForAlls in the type, so that when quantifying an outer for-all we can avoid these
     * inner ones.
     */
    def tyVarBndrs(t: Type): Set[Tyvar] = {
        def bndrs(t: Type, acc: Set[Tyvar]): Set[Tyvar] = {
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
    def tv(t: Type): List[TVar] = t match {
        case u: TVar => List(u)
        case TAp(l, r) => (tv(l) ++ tv(r)).distinct
        case Forall(_, _, t) => tv(t)
        case t => List.empty
    }

    /**
     * Universally quantifies t using the specified type variables.
     */
    def quantify(env: TIEnv, vs: List[TVar], t: Type): (TIEnv, Subst, Type) = {
        val vs1 = tv(t) collect { case v if vs contains v => v }
        if (vs1.isEmpty) (env, nullSubst, t)
        else {
            val ks = vs1 map Kind.kind
            val (env1, fi) = env.newUnique
            val s = (vs1 zip (List.range(0, vs1.size) map { n => TGen(fi, n) })).toMap
            (env1, s, Forall(fi, ks, Substitutions.applySubst(s, t)))
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