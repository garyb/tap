package tap.types.inference

import tap.ast.FilePositional
import tap.ir.TapNode
import tap.types.classes.Qual
import tap.types.kinds.Kind
import tap.types.inference.Substitutions._
import tap.types._
import language.reflectiveCalls

case class TIEnv(uniq: Int, s: Subst, ets: TypeInference.ExprTypeMap) {

    def withCtx[A, B](c: (TIEnv, A), fn: A => B): (TIEnv, B) = (c._1, fn(c._2))

    def newUnique: (TIEnv, Int) = (copy(uniq = uniq + 1), uniq)

    def unify(x: Type, y: Type, src: FilePositional): TIEnv = copy(s = Unify.unify(x, y, s, src))
    def setNodeType(n: TapNode, qt: Qual[Type]): TIEnv = copy(ets = ets + (n -> qt))
    def setNodeType(n: TapNode, t: Type): TIEnv = setNodeType(n, Qual(Nil, t))

    def newTvar(k: Kind): (TIEnv, Type) = withCtx(newMetaTvar(k), { m: Meta => MetaTv(m) })
    def newMetaTvar(k: Kind): (TIEnv, Meta) = withCtx(newUnique, { i: Int => Meta(i, k, None) })
    def newSkolemTvar(tv: Tyvar): (TIEnv, Tyvar) = withCtx(newUnique, { i: Int => SkolemTv(tv.id, i, tv.k) })
    
}

object TIEnv {
    val empty = TIEnv(0, nullSubst, Map.empty)
}