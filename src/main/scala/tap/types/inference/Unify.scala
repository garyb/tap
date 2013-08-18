package tap.types.inference

import Substitutions._
import tap.ast.FilePositional
import tap.types._
import tap.types.kinds.Kind._
import tap.util.PrettyPrint._
import scala.Some

object Unify {

    /**
     * Unifies a variable with a type, ensuring the result is not a recursive type and that the kinds of variable and
     * type match.
     */
    def varBind(u: TVar, t: Type): Option[Subst] =
        if (t == u) Some(nullSubst)
        else if (Type.tv(t) contains u) None //throw TIError("occurs check fails: " + prettyPrint(t) + " contains " + u.id, src)
        else if (kind(u) != kind(t)) None //throw TIError("kinds do not match: " + prettyPrint(u) + " :: " + prettyPrint(kind(u)) + " and " + prettyPrint(t) + " :: " + prettyPrint(kind(t)), src)
        else Some(Map(u -> t))

    def faMatch(i1: Int, i2: Int, fas: List[(Int, Int)]): Boolean =
        (i1 == i2) || (fas exists { case (j1, j2) => (i1 == j1 && i2 == j2) || (i1 == j2 && i2 == j1) })

    /**
     * Attempts to find most general unifier of two types.
     */
    def mgu(x: Type, y: Type, fas: List[(Int, Int)] = Nil): Option[Subst] = (x, y) match {
        case (u: TVar, t) => varBind(u, t)
        case (t, u: TVar) => varBind(u, t)
        case (TAp(l, r), TAp(l1, r1)) => mgu(l, l1, fas) flatMap { s1 => mgu(applySubst(s1, r), applySubst(s1, r1), fas) map { s2 => composeSubst(s2, s1) } }
        case (tc1: TCon, tc2: TCon) if tc1 == tc2 => Some(nullSubst)
        case (TGen(fa1, i1), TGen(fa2, i2)) if faMatch(fa1, fa2, fas) && i1 == i2 => Some(nullSubst)
        case (Forall(i1, ks1, t1), Forall(i2, ks2, t2)) if ks1 == ks2 => mgu(t1, t2, (i1 -> i2) :: fas)
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
    def `match`(t1: Type, t2: Type, fas: List[(Int, Int)] = Nil): Option[Subst] = (t1, t2) match {
        case (u: TVar, t) if kind(u) == kind(t) => Some(Map(u -> t))
        case (TAp(l, r), TAp(l1, r1)) => `match`(l, l1, fas) flatMap { sl => `match`(r, r1, fas) flatMap { sr => merge(sl, sr) } }
        case (tc1: TCon, tc2: TCon) if tc1 == tc2 => Some(nullSubst)
        case (TGen(fa1, i1), TGen(fa2, i2)) if faMatch(fa1, fa2, fas) && i1 == i2 => Some(nullSubst)
        case (Forall(i1, ks1, t1), Forall(i2, ks2, t2)) if ks1 == ks2 => `match`(t1, t2, (i1 -> i2) :: fas)
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
