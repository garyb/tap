package tap.types.inference

import tap.ir.TapNode
import tap.types.classes.Qual
import tap.types.kinds.{Star, Kind}
import tap.types.kinds.Kind._
import tap.types.classes.ClassEnvironments.ClassEnv
import tap.types._
import tap.util.ContextOps._
import language.reflectiveCalls

case class TIEnv(uniq: Int, subst: Map[Meta, Type], ce: ClassEnv, ntm: Map[TapNode, Qual[Type]]) {

    def withCtx[A, B](c: (TIEnv, A), fn: A => B): (TIEnv, B) = (c._1, fn(c._2))

    // ---[ substitutions ]----------------------------------------------------

    def applySubst(t: Type): Type = t match {
        case MetaTv(m) => subst.getOrElse(m, t)
        case _ => t
    }

    def substTv(tvs: List[TyVar], ts: List[Type], t: Type) =
        Substitutions.applySubst(tvs, ts, t)

    // ---[ type variables ]---------------------------------------------------

    def newTvar(k: Kind): (TIEnv, Type) =
        withCtx(newMetaTvar(k), { m: Meta => MetaTv(m) })

    def newMetaTvar(k: Kind): (TIEnv, Meta) =
        withCtx(newUnique, { i: Int => Meta(i, k) })

    def newSkolemTvar(tv: TyVar): (TIEnv, TyVar) =
        withCtx(newUnique, { i: Int => SkolemTv(tv.id, i, tv.k) })

    def readTv(m: Meta): Option[Type] = subst.get(m)
    def writeTv(m: Meta, t: Type): TIEnv = copy(subst = subst + (m -> t))

    def newUnique: (TIEnv, Int) = (copy(uniq = uniq + 1), uniq)

    // ---[ instantiation ]----------------------------------------------------

    def instantiate(t: Type): (TIEnv, Type) = t match {
        case Forall(tvs, t) =>
            val (env1, tvs1) = this.map(tvs map kind) { case (env, k) => env.newMetaTvar(k) }
            (env1, substTv(tvs, tvs1 map MetaTv.apply, t))
        case _ => (this, t)
    }

    // ---[ skolemisation ]----------------------------------------------------

    def skolemise(t: Type): (TIEnv, List[TyVar], Type) = t match {
        case Forall(tvs, t) =>
            val (env1, sks1) = this.map(tvs) { case (env, tv) => env.newSkolemTvar(tv) }
            val (env2, sks2, t1) = env1.skolemise(substTv(tvs, sks1 map TVar.apply, t))
            (env2, sks1 ++ sks2, t1)
        case TFun(x, y) =>
            val (env1, sks, y1) = skolemise(y)
            (env1, sks, x fn y1)
        case _ => (this, Nil, t)
    }

    // ---[ quantification ]---------------------------------------------------

    def quantify(tvs: List[Meta], t: Type): (TIEnv, Type) = {

        val usedBndrs = Type.tyVarBndrs(t) map { tv => tv.id }
        val newBnds = allBinders filterNot usedBndrs.contains take tvs.length

        val tvs1 = Type.mtv(t) collect { case v if tvs contains v => v }
        val (env1, tvs2) = this.map(tvs1 zip newBnds) { case (env0, (m, name)) =>
            val btv = BoundTv(name, m.k)
            val env1 = env0.writeTv(m, TVar(btv))
            (env1, btv)
        }
        val (env2, t1) = env1.zonkType(t)
        (env2, Forall(tvs2, t1))
    }

    val allBinders = for (i <- Stream.from(0); x <- ('a' to 'z').iterator)
        yield if (i == 0) x.toString else x.toString + i

    // ---[ free type variables ]----------------------------------------------

    def getMetaTyVars(ts: List[Type]): (TIEnv, List[Meta]) = {
        val (env1, ts1) = zonkTypes(ts)
        (env1, (ts1 flatMap Type.mtv).distinct)
    }

    def getFreeTyVars(ts: List[Type]): (TIEnv, List[TyVar]) = {
        def find(bound: Set[TyVar], t: Type): List[TyVar] = t match {
            case Forall(tvs, t) => find(bound ++ tvs, t)
            case TAp(x, y) => (find(bound, x) ++ find(bound, y)).distinct
            case TVar(tv) if !(bound contains tv) => List(tv)
            case _ => Nil
        }
        val (env1, ts1) = zonkTypes(ts)
        val tvs = ts1 flatMap { t => find(Set.empty, t) }
        (env1, tvs)
    }

    // ---[ zonking ]----------------------------------------------------------

    def zonkType(t: Type): (TIEnv, Type) = t match {
        case Forall(ns, t) => withCtx(zonkType(t), t => Forall(ns, t))
        case TAp(x, y) =>
            val (ctx1, x1) = zonkType(x)
            val (ctx2, y1) = ctx1.zonkType(y)
            (ctx2, TAp(x1, y1))
        case MetaTv(m) => readTv(m) match {
            case None => (this, t)
            case Some(t) =>
                val (ctx1, t1) = zonkType(t)
                val ctx2 = ctx1.writeTv(m, t1)
                (ctx2, t1)
        }
        case t => (this, t)
    }

    def zonkTypes(ts: List[Type]): (TIEnv, List[Type]) =
        this.map(ts) { case (env0, t) => env0.zonkType(t) }

    // ---[ unification ]------------------------------------------------------

    def unify(x: Type, y: Type): TIEnv = (x, y) match {
        case (TVar(_: BoundTv), _) => throw TIInternalError("Unexpected type in unify " + x)
        case (_, TVar(_: BoundTv)) => throw TIInternalError("Unexpected type in unify " + y)
        case (t1, t2) if t1 == t2 => this
        case (MetaTv(tv), t) => unifyVar(tv, t)
        case (t, MetaTv(tv)) => unifyVar(tv, t)
        case (TAp(x1, y1), TAp(x2, y2)) => unify(x1, x2).unify(y1, y2)
        case _ => throw TIUnifyError(x, y)
    }

    def unifyVar(tv1: Meta, t2: Type): TIEnv = {
        if (kind(tv1) != kind(t2)) throw TIUnifyKindError(tv1, t2)
        readTv(tv1) match {
            case Some(t1) => unify(t1, t2)
            case None => unifyUnboundVar(tv1, t2)
        }
    }

    def unifyUnboundVar(tv1: Meta, t2: Type): TIEnv = t2 match {
        case MetaTv(tv2) =>
            readTv(tv2) match {
                case Some(tv2a) => unify(MetaTv(tv1), tv2a)
                case None => writeTv(tv1, t2)
            }
        case t2 =>
            val (env1, tvs) = getMetaTyVars(List(t2))
            if (tvs contains tv1) throw TIUnifyOccursError(tv1, t2)
            env1.writeTv(tv1, t2)
    }

    def unifyFun(t: Type): (TIEnv, Type, Type) = t match {
        case TFun(x, y) => (this, x, y)
        case t =>
            val (env1, x) = newTvar(Star)
            val (env2, y) = env1.newTvar(Star)
            val env3 = env2.unify(t, x fn y)
            (env3, x, y)
    }

    // ---[ node type map ]----------------------------------------------------

    def setNodeType(n: TapNode, qt: Qual[Type]): TIEnv = copy(ntm = ntm + (n -> qt))
    def setNodeType(n: TapNode, t: Type): TIEnv = setNodeType(n, Qual(Nil, t))
}

object TIEnv {
    val empty = TIEnv(0, Map.empty, Map.empty, Map.empty)
}