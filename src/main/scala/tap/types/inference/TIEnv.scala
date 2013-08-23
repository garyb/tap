package tap.types.inference

import tap.ast.FilePositional
import tap.ir.TapNode
import tap.types.classes.Qual
import tap.types.kinds.Kind
import tap.types._
import tap.util.ContextOps._
import language.reflectiveCalls
import tap.Id

case class TIEnv(uniq: Int, env: Map[Id, Type], ets: TypeInference.ExprTypeMap) {

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

    def lookupVar(id: Id): Type = env(id)
    def extendVarEnv[A](id: Id, t: Type, fn: TIEnv => A): (TIEnv, A) = {
        (this, fn(copy(env = env + (id -> t))))
    }

    def getEnvTypes: (TIEnv, List[Type]) = (this, env.values.toList)

    private def isValidType(t: Type): Boolean = t match {
        case TVar(_: BoundTv) => false
        case _ => true
    }

    def unify(x: Type, y: Type, src: FilePositional): TIEnv = {
        assert(isValidType(x), "bad type " + x)
        assert(isValidType(y), "bad type " + x)
        (x, y) match {
            case (tv1: TVar, tv2: TVar) if tv1 == tv2 => this
            case (tv1: MetaTv, tv2: MetaTv) if tv1 == tv2 => this
            case (tc1: TCon, tc2: TCon) if tc1 == tc2 => this
            case (MetaTv(tv), t) => unifyVar(tv, t, src)
            case (t, MetaTv(tv)) => unifyVar(tv, t, src)
            case (TAp(x1, y1), TAp(x2, y2)) => unify(x1, x2, src).unify(y1, y2, src)
            case _ => throw new TIError("could not unify types", src)
        }
    }

    def unifyVar(tv1: Meta, t2: Type, src: FilePositional): TIEnv = tv1.ref match {
        case Some(t1) => unify(t1, t2, src)
        case None => unifyUnboundVar(tv1, t2, src)
    }

    def unifyUnboundVar(tv1: Meta, t2: Type, src: FilePositional): TIEnv = t2 match {
        case MetaTv(tv2) =>
            tv2.ref match {
                case Some(tv2a) => unify(MetaTv(tv1), tv2a, src)
                case None =>
                    tv1.ref = Some(t2)
                    this
            }
        case t2 =>
            val tvs2 = Type.getMetaTyVars(List(t2))
            if (tvs2 contains tv1) throw TIError("Occurs check failed for " + tv1 + " in " + t2, src)
            tv1.ref = Some(t2)
            this
    }

}

object TIEnv {
    val empty = TIEnv(0, Map.empty, Map.empty)
}