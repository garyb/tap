package tap.types.inference

import tap.util.ContextOps._
import tap.ast.FilePositional
import tap.ir.TapNode
import tap.types.classes.Qual
import tap.types.kinds.Kind
import tap.types.inference.Substitutions._
import tap.util.PrettyPrint._
import tap.types._
import language.reflectiveCalls

case class TIEnv(uniq: Int, s: Subst, ets: TypeInference.ExprTypeMap) {

    def withCtx[A, B](c: (TIEnv, A), fn: A => B): (TIEnv, B) = (c._1, fn(c._2))

    def newUnique: (TIEnv, Int) = (copy(uniq = uniq + 1), uniq)

    def unify(x: Type, y: Type, src: FilePositional): TIEnv = copy(s = Unify.unify(x, y, s, src))
    def setNodeType(n: TapNode, qt: Qual[Type]): TIEnv = copy(ets = ets + (n -> qt))
    def setNodeType(n: TapNode, t: Type): TIEnv = setNodeType(n, Qual(Nil, t))

    def newTVar(k: Kind): (TIEnv, Type) = withCtx(newUnique, { i: Int => TVar("µ" + i, k) })

    def freshInst(s: Qual[Type]): (TIEnv, Qual[Type]) = freshInstPartial(Nil, s)

    def freshInst(s: Type): (TIEnv, Type) = freshInstPartial(Nil, s)

    def freshInstPartial(ts0: List[Type], s: Qual[Type]): (TIEnv, Qual[Type]) = s match {
        case Qual(ps, sc @ Forall(_, ks, t)) =>
            if (ts0.length > ks.length) throw TIInternalError("Too many parameters in freshInstPartial: " + (ts0 map prettyPrint mkString ", ") + " going into " + prettyPrint(sc))
            val (ctx, ts1) = this.map(ks.drop(ts0.size)) { (ctx, k) => newTVar(k) }
            (ctx, Qual.inst(sc, ts0 ++ ts1, Qual(ps, t)))
        case s => (this, s)
    }

    def freshInstPartial(ts0: List[Type], s: Type): (TIEnv, Type) = s match {
        case sc @ Forall(_, ks, t) =>
            if (ts0.length > ks.length) throw TIInternalError("Too many parameters in freshInstPartial: " + (ts0 map prettyPrint mkString ", ") + " going into " + prettyPrint(sc))
            val (ctx, ts1) = this.map(ks.drop(ts0.size)) { (ctx, k) => newTVar(k) }
            (ctx, Type.inst(sc, ts0 ++ ts1, t))
        case s => (this, s)
    }
}

object TIEnv {
    val empty = TIEnv(0, nullSubst, Map.empty)
}