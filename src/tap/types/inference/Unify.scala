package tap.types.inference

import Substitutions._
import tap.ast.FilePositional
import tap.types._
import tap.types.kinds.Kind._
import tap.util.PrettyPrint._
import scala.Some
import tap.util.trace

object Unify {

    /**
     * Unifies a variable with a type, ensuring the result is not a recursive type and that the kinds of variable and
     * type match.
     */
    def varBind(u: Tyvar, t: Type): Option[Subst] =
        if (t == TVar(u)) Some(nullSubst)
        else if (tv(t) contains u) None //throw TIError("occurs check fails: " + prettyPrint(t) + " contains " + u.id, src)
        else if (kind(u) != kind(t)) None //throw TIError("kinds do not match: " + prettyPrint(u) + " :: " + prettyPrint(kind(u)) + " and " + prettyPrint(t) + " :: " + prettyPrint(kind(t)), src)
        else Some(Map(u -> t))

    /**
     * Attempts to find most general unifier of two types.
     */
    def mgu(x: Type, y: Type): Option[Subst] = (x, y) match {
        case (TVar(u), t) => varBind(u, t)
        case (t, TVar(u)) => varBind(u, t)
        case (TAp(l, r), TAp(l1, r1)) => mgu(l, l1) flatMap { s1 => mgu(applySubst(s1, r), applySubst(s1, r1)) map { s2 => composeSubst(s2, s1) } }
        case (TCon(tc1), TCon(tc2)) if tc1 == tc2 => Some(nullSubst)
        case (TGen(ki1, i1), TGen(ki2, i2)) if ki1 == ki2 && i1 == i2 => Some(nullSubst)
        case (Forall(i1, tvs1, t1), Forall(i2, tvs2, t2)) if i1 == i2 && tvs1 == tvs2 => mgu(t1, t2)
        case _ => None
    }

    def mgus(xs: List[Type], ys: List[Type]): Option[Subst] = (xs, ys) match {
        case (x :: xs, y :: ys) =>
            mgu(x, y) flatMap { s1 =>
            mgus(xs map { x => applySubst(s1, x) }, ys map { y => applySubst(s1, y) }) map { s2 =>
            composeSubst(s2, s1) }}
        case (List(), List()) => Some(nullSubst)
        case _ => None
    }

    /**
     * Extend the current substitution with the most general unifier of two types.
     */
    def unify(x: Type, y: Type, s: Subst, src: FilePositional): Subst =
        mgu(applySubst(s, x), applySubst(s, y)) match {
            case Some(u) => composeSubst(u, s)
            case None => throw TIError("types do not unify:\n    " + prettyPrint(applySubst(s, x)) + "\n    " + prettyPrint(applySubst(s, y)), src)
        }

    /**
     * Attempts to find a substitution that when applied to t1 results in t2.
     */
    def `match`(t1: Type, t2: Type): Option[Subst] = (t1, t2) match {
        case (TVar(u), t) if kind(u) == kind(t) => Some(Map(u -> t))
        case (TAp(l, r), TAp(l1, r1)) => `match`(l, l1) flatMap { sl => `match`(r, r1) flatMap { sr => merge(sl, sr) } }
        case (TCon(tc1), TCon(tc2)) if tc1 == tc2 => Some(nullSubst)
        case (TGen(ki1, i1), TGen(ki2, i2)) if ki1 == ki2 && i1 == i2 => Some(nullSubst)
        case (Forall(i1, tvs1, t1), Forall(i2, tvs2, t2)) if i1 == i2 && tvs1 == tvs2 => `match`(t1, t2)
        case _ => None
    }

    def matches(xs: List[Type], ys: List[Type]): Option[Subst] = {
        if (xs.length != ys.length) None
        else {
            val ms = (xs zip ys).map { case (x, y) => `match`(x, y) }
            ms.foldRight(Option(List.empty[Subst])) { case (s, result) =>
                result flatMap { xs =>
                    s match {
                        case Some(x) => Some(x :: xs)
                        case _ => None
                    }
                }
            } flatMap { ss =>
                ss.foldLeft(Option(nullSubst)) { (result, x) =>
                    result flatMap { a => merge(a, x) }
                }
            }
        }
    }
}
