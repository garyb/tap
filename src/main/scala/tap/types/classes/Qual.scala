package tap.types.classes

import tap.types.{TGen, TVar, Forall, Type}
import tap.types.inference.Substitutions
import tap.types.kinds.Kind
import tap.util.trace

case class Qual[+T](ps: List[IsIn], h: T)

object Qual {

    def inst(ts: List[Type], sc: Qual[Forall]): Qual[Type] =
        inst(sc.h, ts, sc)

    def inst(sc: Forall, ts: List[Type], p: Qual[Type]): Qual[Type] =
        Qual(p.ps map { p => IsIn.inst(sc, ts, p) }, Type.inst(sc, ts, p.h))

    def quantify(vs: List[TVar], t: Qual[Type], fi: Option[Int] = None): Qual[Type] = {
        val vs1 = Substitutions.tv(t) collect { case v if vs contains v => v }
        if (vs1.isEmpty) t
        else {
            val ks = vs1 map Kind.kind
            val fi1 = fi.getOrElse(Type.newForallId())
            val s = (vs1 zip (List.range(0, vs1.size) map { n => TGen(fi1, n) })).toMap
            Qual(t.ps map { p => Substitutions.applySubst(s, p) }, Forall(fi1, ks, Substitutions.applySubst(s, t.h)))
        }
    }
}
