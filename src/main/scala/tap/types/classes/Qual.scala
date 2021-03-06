package tap.types.classes

import tap.types.{TGen, TVar, Forall, Type}
import tap.types.inference.{TIEnv, Substitutions}
import tap.types.inference.Substitutions.{nullSubst, Subst}
import tap.types.kinds.Kind

case class Qual[+T](ps: List[IsIn], h: T)

object Qual {

    /**
     * Finds the type variables used within a qualified type.
     */
    def tv(qt: Qual[Type]): List[TVar] = ((qt.ps flatMap IsIn.tv) ++ Type.tv(qt.h)).distinct

    def inst(ts: List[Type], sc: Qual[Forall]): Qual[Type] =
        inst(sc.h, ts, sc)

    def inst(sc: Forall, ts: List[Type], p: Qual[Type]): Qual[Type] =
        Qual(p.ps map { p => IsIn.inst(sc, ts, p) }, Type.inst(sc, ts, p.h))

    def quantify(env: TIEnv, vs: List[TVar], t: Qual[Type], fi: Option[Int] = None): (TIEnv, Subst, Qual[Type]) = {
        val vs1 = tv(t) collect { case v if vs contains v => v }
        if (vs1.isEmpty) (env, nullSubst, t)
        else {
            val ks = vs1 map Kind.kind
            val (env1, fi1) = fi match {
                case Some(fi) => (env, fi)
                case None => env.newUnique
            }
            val s = (vs1 zip (List.range(0, vs1.size) map { n => TGen(fi1, n) })).toMap
            (env1, s, Qual(t.ps map { p => Substitutions.applySubst(s, p) }, Forall(fi1, ks, Substitutions.applySubst(s, t.h))))
        }
    }
}
