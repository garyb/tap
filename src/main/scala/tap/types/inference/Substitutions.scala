package tap.types.inference

import tap.types._
import tap.types.classes.{Qual, IsIn}

object Substitutions {

    type Subst = Map[TVar, Type]

    def nullSubst: Subst = Map.empty

    /**
     * Applies a substitution to a type.
     */
    def applySubst(s: Subst, t: Type): Type = t match {
        case Forall(tvs, t) => Forall(tvs, applySubst(s filterKeys { tv => !(tvs contains tv) }, t))
        case TAp(l, r) => TAp(applySubst(s, l), applySubst(s, r))
        case u: TVar => s.getOrElse(u, u)
        case t => t
    }

    def applySubst(tvs: List[TVar], ts: List[Type], t: Type): Type = applySubst((tvs zip ts).toMap, t)

    /**
     * Applies a substitution to a predicated type.
     */
    def applySubst(s: Subst, t: IsIn): IsIn = IsIn(t.id, t.ts map { t => applySubst(s, t) })

    /**
     * Applies a substitution to a qualified type.
     */
    def applySubst(s: Subst, qt: Qual[Type]): Qual[Type] = Qual(qt.ps map { applySubst(s, _) }, applySubst(s, qt.h))

    /**
     * Composes two substitutions.
     */
    def composeSubst(s1: Subst, s2: Subst) =
        s2.collect { case (u, t) => (u, applySubst(s1, t)) } ++ s1

    /**
     * Combines two substitutions checking that any overlapping variables agree in both substitutions.
     */
    def merge(s1: Subst, s2: Subst): Option[Subst] = {
        val tvars = s1.keySet intersect s2.keySet
        val agree = tvars forall { v => applySubst(s1, v) == applySubst(s2, v) }
        if (agree) Some(s1 ++ s2) else None
    }
}