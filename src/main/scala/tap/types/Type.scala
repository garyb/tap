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
case class Forall(tvs: List[Tyvar], t: Type) extends Type
case class TAp(f: Type, a: Type) extends Type
case class TCon(id: ModuleId, k: Kind) extends Type
case class TVar(tv: Tyvar) extends Type { def id: String = tv.id }
case class MetaTv(m: Meta) extends Type

sealed trait Tyvar {
    def id: String
    def k: Kind
}
case class BoundTv(id: String, k: Kind) extends Tyvar
case class SkolemTv(id: String, i: Int, k: Kind) extends Tyvar

case class Meta(i: Int, k: Kind, var ref: Option[Type])

object Type {

    /**
     * Get the MetaTvs from a type.
     */
    def metaTvs(ts: List[Type]): List[MetaTv] = {
        ts.flatMap(tv).distinct
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
    def tv(t: Type): List[MetaTv] = t match {
        case Forall(_, t) => tv(t)
        case TAp(x, y) => (tv(x) ++ tv(y)).distinct
        case m: MetaTv => List(m)
        case _ => List.empty
    }

    val allBinders = for (i <- Stream.from(0); x <- ('a' to 'z').iterator) yield if (i == 0) x.toString else x.toString + i

    def zonkType(t: Type): Type = t match {
        case Forall(ns, t) => Forall(ns, zonkType(t))
        case TAp(x, y) => TAp(zonkType(x), zonkType(y))
        case MetaTv(m @ Meta(i, k, Some(t))) =>
            val t1 = zonkType(t)
            m.ref = Some(t1)
            t1
        case t => t
    }

    /**
     * Universally quantifies t using the specified type variables.
     */
    def quantify(vs: List[MetaTv], t: Type): Type = {

        val usedBndrs = tyVarBndrs(t) map { tv => tv.id }
        val newBnds = allBinders filterNot usedBndrs.contains take vs.length

        def bind(tv: MetaTv, name: String): Tyvar = {
            val btv = BoundTv(name, tv.m.k)
            tv.m.ref = Some(TVar(btv))
            btv
        }

        val vs1 = tv(t) collect { case v if vs contains v => v }
        if (vs1.isEmpty) t
        else Forall((vs1 zip newBnds) map { case (tv, n) => bind(tv, n) }, zonkType(t))
    }

    /**
     * Instantiates a universally quantified type, replacing the quantified types with other types.
     */
    def inst(env0: TIEnv, t: Type): (TIEnv, Type) = t match {
        case Forall(vs, t) =>
            val (env1, vs1) = env0.map(vs) { case (env0, tv) => env0.newMetaTvar(tv.k) }
            (env1, Substitutions.applySubst(vs, vs1 map MetaTv.apply, t))
        case t => (env0, t)
    }
}