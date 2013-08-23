package tap.types.inference

import tap.ast.FilePositional
import tap.ir.TapNode
import tap.types.classes.Qual
import tap.types.kinds.Kind
import tap.types._
import tap.util.ContextOps._
import language.reflectiveCalls

case class TIEnv(uniq: Int, env: Map[String, Type], ets: TypeInference.ExprTypeMap) {

    def withCtx[A, B](c: (TIEnv, A), fn: A => B): (TIEnv, B) = (c._1, fn(c._2))

    def newUnique: (TIEnv, Int) = (copy(uniq = uniq + 1), uniq)

    //def unify(x: Type, y: Type, src: FilePositional): TIEnv = copy(s = Unify.unify(x, y, s, src))
    def setNodeType(n: TapNode, qt: Qual[Type]): TIEnv = copy(ets = ets + (n -> qt))
    def setNodeType(n: TapNode, t: Type): TIEnv = setNodeType(n, Qual(Nil, t))

    def newTvar(k: Kind): (TIEnv, Type) = withCtx(newMetaTvar(k), { m: Meta => MetaTv(m) })
    def newMetaTvar(k: Kind): (TIEnv, Meta) = withCtx(newUnique, { i: Int => Meta(i, k, None) })
    def newSkolemTvar(tv: TyVar): (TIEnv, TyVar) = withCtx(newUnique, { i: Int => SkolemTv(tv.id, i, tv.k) })

    /**
     * Instantiates a universally quantified type, replacing the quantified types with other types.
     */
    def inst(t: Type): (TIEnv, Type) = t match {
        case Forall(vs, t) =>
            val (env1, vs1) = this.map(vs) { case (env0, tv) => env0.newMetaTvar(tv.k) }
            (env1, Substitutions.applySubst(vs, vs1 map MetaTv.apply, t))
        case t => (this, t)
    }

    /**
     * Performs deep skolemisation, returning the skolem constants and newly skolemised type.
     */
    def skolemise(t: Type): (TIEnv, List[TyVar], Type) = t match {
        case Forall(vs, t0) =>
            val (env1, sks1) = this.map(vs) { case (env0, tv) => env0.newSkolemTvar(tv) }
            val (env2, sks2, t1) = env1.skolemise(Substitutions.applySubst(vs, sks1 map TVar.apply, t0))
            (env2, sks1 ++ sks2, t1)
        case TAp(x, y0) =>
            val (env1, tvs, y1) = skolemise(y0)
            (env1, tvs, TAp(x, y1))
        case t => (this, Nil, t)
    }

    def lookupVar(name: String): Type = env(name)

    def getEnvTypes: (TIEnv, List[Type]) = (this, env.values.toList)



}

object TIEnv {
    val empty = TIEnv(0, Map.empty, Map.empty)
}