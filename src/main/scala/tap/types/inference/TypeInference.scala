package tap.types.inference

import Substitutions._
import tap.ast.FilePositional
import tap.util.{trace, ContextOps}
import ContextOps._
import tap.ir._
import tap.types.Type._
import tap.types._
import tap.types.classes.ClassEnvironments.ClassEnv
import tap.types.classes.IsIn.{reduce, entail, split}
import tap.types.classes._
import tap.types.kinds.{Kind, Star}
import tap.util.PrettyPrint._
import tap.{LocalId, Id}
import language.reflectiveCalls

object TypeInference {

    case class Context(s: Subst, ets: ExprTypeMap) {
        def unify(x: Type, y: Type, src: FilePositional): Context = Context(Unify.unify(x, y, s, src), ets)
        def setNodeType(n: TapNode, qt: Qual[Type]): Context = Context(s, ets + (n -> qt))
        def setNodeType(n: TapNode, t: Type): Context = setNodeType(n, Qual(Nil, t))
    }

    type Assumps = Map[Id, Qual[Type]]

    /**
     * Creates a new type variable based on the current state.
     */
    def newTVar(k: Kind): Type = {
        tvId += 1
        TVar("µ" + tvId, k)
    }
    var tvId = 0

    def freshInst(s: Qual[Type]): Qual[Type] = s match {
        case Qual(ps, sc @ Forall(_, ks, t)) => Qual.inst(sc, ks map newTVar, Qual(ps, t))
        case s => s
    }

    def freshInst(s: Type): Type = s match {
        case sc @ Forall(_, ks, t) => Type.inst(sc, ks map newTVar, t)
        case s => s
    }

    def freshInstPartial(ts0: List[Type], s: Qual[Type]): Qual[Type] = s match {
        case Qual(ps, sc @ Forall(_, ks, t)) =>
            val ts1 = ks.drop(ts0.size) map newTVar
            Qual.inst(sc, ts0 ++ ts1, Qual(ps, t))
        case s => s
    }

    def freshInstPartial(ts0: List[Type], s: Type): Type = s match {
        case sc @ Forall(_, ks, t) =>
            val ts1 = ks.drop(ts0.size) map newTVar
            Type.inst(sc, ts0 ++ ts1, t)
        case s => s
    }

    def toQual(ps: List[IsIn], t: Type): Qual[Type] = {
        val tvs = tv(t)
        val ps1 = ps filter { p => tv(p) forall { tv => tvs contains tv } }
        Qual(ps1, t)
    }

    // -----------------------------------------------------------------------------------------------------------
    //  Type inference
    // -----------------------------------------------------------------------------------------------------------

    type ExprTypeMap = Map[TapNode, Qual[Type]]

    /**
     * Builds the constraints for inferring the type of an expression.
     */
    def tiExpr(ce: ClassEnv, as: Assumps, ctx: Context, node: TapExpr, explArgs: List[Type]): (Context, List[IsIn], Type) = node match {

        case BlockExpr(es) =>
            def traverse(ctx: Context, ps: List[IsIn], es: Seq[TapExpr], result: Type): (Context, List[IsIn], Type) = es match {
                case e :: es =>
                    val (ctx1, qs, et) = tiExpr(ce, as, ctx, e, explArgs)
                    traverse(ctx1, ps ++ qs, es, et)
                case Seq() =>
                    (ctx.setNodeType(node, result), ps, result)
            }
            traverse(ctx, Nil, es, tUnit)

        case ApplyExpr(f, e) =>
            val (ctx1, ps, tf) = tiExpr(ce, as, ctx,  f, explArgs)
            //val teArgs = getFunctionTypeArgs(tf)
            val (ctx2, qs, te) = tiExpr(ce, as, ctx1, e, explArgs)
            val t = newTVar(Star)
            val ctx3 = ctx2.unify(te fn t, tf, node)
            (ctx3.setNodeType(node, t), ps ++ qs, t)

        case MatchExpr(e, branches) =>
            val (ctx1, ps, t) = tiExpr(ce, as, ctx, e, explArgs)
            val v = newTVar(Star)
            val (ctx2, qs) = ctx1.flatMap(branches) { case (ctx, cn) => tiBranch(ce, as, ctx, t, v, cn, explArgs) }
            (ctx2.setNodeType(node, v), ps ++ qs, v)

        case LetExpr(name, e, f) =>
            val t = newTVar(Star)
            val as1 = as + (LocalId(name) -> Qual(Nil, t))
            val (ctx1, ps, et) = tiExpr(ce, as1, ctx, e, explArgs)
            val ctx2 = ctx1.unify(t, et, node)
            val et1 = applySubst(ctx2.s, et)
            val as2 = as + (LocalId(name) -> toQual(ps, et1))
            val (ctx3, qs, ft) = tiExpr(ce, as2, ctx2, f, explArgs)
            (ctx3.setNodeType(node, ft), ps ++ qs, ft)

        case ValueReadExpr(i) =>
            val qt = freshInst(as(i))
            (ctx.setNodeType(node, qt), qt.ps, qt.h)

        case CastExpr(e, ct) =>
            val (ctx1, ps, et) = tiExpr(ce, as, ctx, e, explArgs)
            val ctx2 = ctx1.unify(et, ct, e)
            (ctx2.setNodeType(node, Qual(ps, ct)), ps, ct)

        case _: StringExpr => (ctx.setNodeType(node, tString), Nil, tString)
        case _: NumberExpr => (ctx.setNodeType(node, tNumber), Nil, tNumber)

        case FunctionExpr(NoArgument, b) if explArgs.nonEmpty =>
            val ctx1 = ctx.unify(tUnit, explArgs.head, node)
            val (ctx2, ps, t) = tiExpr(ce, as, ctx1, b, explArgs.tail)
            val bt = tUnit fn t
            (ctx2.setNodeType(node, bt), ps, bt)

        case FunctionExpr(argNode @ Argument(arg), b) if explArgs.nonEmpty =>
            val at = explArgs.head
            val as1 = as + (LocalId(arg) -> Qual(Nil, at))
            val (ctx1, ps, rt) = tiExpr(ce, as1, ctx, b, explArgs.tail)
            val bt = at fn rt
            (ctx1.setNodeType(node, bt).setNodeType(argNode, at), ps, bt)

        case FunctionExpr(NoArgument, b) =>
            val (ctx1, ps, t) = tiExpr(ce, as, ctx, b, explArgs)
            val bt = tUnit fn t
            (ctx1.setNodeType(node, bt), ps, bt)

        case FunctionExpr(argNode @ Argument(arg), b) =>
            val at = newTVar(Star)
            val as1 = as + (LocalId(arg) -> Qual(Nil, at))
            val (ctx1, ps, rt) = tiExpr(ce, as1, ctx, b, explArgs)
            val bt = at fn rt
            (ctx1.setNodeType(node, bt).setNodeType(argNode, at), ps, bt)

        case NativeValueExpr(ref, t) =>
            (ctx.setNodeType(node, t), Nil, t)

        case RaiseErrorExpr(e) =>
            val (ctx1, ps, te) = tiExpr(ce, as, ctx, e, explArgs)
            val ctx2 = ctx1.unify(te, tString, e)
            val t = newTVar(Star)
            (ctx2.setNodeType(node, t), ps, t)
    }

    def tiBranch(ce: ClassEnv, as: Assumps, ctx: Context, t: Type, v: Type, node: MatchCase, explArgs: List[Type]): (Context, List[IsIn]) = node match {

        case MatchCase(pat, None, f) =>
            val (as1, ctx1, ps, t1) = tiPattern(as, ctx, pat)
            val (ctx2, qs, t2) = tiExpr(ce, as1, ctx1, f, explArgs)
            val ctx3 = ctx2.unify(t1, t, pat)
                           .unify(t2, v, f)
            (ctx3.setNodeType(node, t), ps ++ qs)

        case MatchCase(pat, Some(g), f) =>
            val (as1, ctx1, ps, t1) = tiPattern(as, ctx, pat)
            val (ctx2, rs, t3) = tiExpr(ce, as1, ctx1, g, explArgs)
            val (ctx3, qs, t2) = tiExpr(ce, as1, ctx2, f, explArgs)
            val ctx4 = ctx3.unify(t1, t, pat)
                           .unify(t3, tBool, g)
                           .unify(t2, v, f)
            (ctx4.setNodeType(node, t), ps ++ rs ++ qs)
    }

    def tiPattern(as: Assumps, ctx: Context, node: PatternNode): (Assumps, Context, List[IsIn], Type) = node match {

        case UnapplyNode(name, args) =>
            val (as1, ctx1, ps, ts) = tiPatterns(as, ctx, args)
            val t1 = newTVar(Star)
            val Qual(qs, t) = freshInst(as1(name))
            val t2 = ts.foldRight(t1) { _ fn _ }
            val ctx2 = ctx1.unify(t, t2, node)
            (as1, ctx2.setNodeType(node, t1), ps ++ qs, t1)

        case BindNode(name, None) =>
            val v = newTVar(Star)
            (as + (LocalId(name) -> Qual(Nil, v)), ctx.setNodeType(node, v), Nil, v)

        case BindNode(name, Some(pat)) =>
            val (as1, ctx1, ps, t) = tiPattern(as, ctx, pat)
            (as1 + (LocalId(name) -> Qual(Nil, t)), ctx1.setNodeType(node, t), ps, t)

        case _: StringExpr => (as, ctx.setNodeType(node, tString), Nil, tString)
        case _: NumberExpr => (as, ctx.setNodeType(node, tNumber), Nil, tNumber)

        case WildcardValueExpr =>
            val v = newTVar(Star)
            (as, ctx.setNodeType(node, v), Nil, v)
    }

    def tiPatterns(as: Assumps, ctx: Context, pats: List[PatternNode]): (Assumps, Context, List[IsIn], List[Type]) =
        pats.foldRight((as, ctx, List.empty[IsIn], List.empty[Type])) { case (pat, (as, ctx, ps, ts)) =>
            val (as1, ctx1, qs, t) = tiPattern(as, ctx, pat)
            (as1, ctx1, ps ++ qs, t :: ts)
        }

    // -----------------------------------------------------------------------------------------------------------
    //  Binding Groups
    // -----------------------------------------------------------------------------------------------------------

    type Expl = (Id, Qual[Type], TapExpr)
    type Impl = (Id, TapExpr)
    type BindGroup = (List[Expl], List[List[Impl]])
    type Infer[E] = (ClassEnv, Assumps, Context, E) => (Assumps, Context, List[IsIn])

    /**
     * Type infers a definition and checks it against an expected type. Used by both implicit and explicitly typed
     * definitions, in the case of implicit definitions t will be a TVar so the check will always pass. Returns a list
     * of predicates that were accumulated from e.
     */
    def tiDef(ce: ClassEnv, as: Assumps, ctx: Context, e: TapExpr, t: Type): (Context, List[IsIn]) = {
        val (ctx1, qs, t1) = tiExpr(ce, as, ctx, e, getFunctionTypeArgs(t))
        val ctx2 = ctx1.unify(t, t1, e)
        (ctx2.setNodeType(e, Qual(qs, t)), qs)
    }

    /**
     * Type checks an explicitly typed definition.
     */
    def tiExpl(ce: ClassEnv, as: Assumps, ctx: Context, ex: Expl): (Context, List[IsIn]) = ex match {
        case (id, sc, e) =>
            val Qual(qs, t) = freshInst(sc)
            val (ctx1, ps) = tiDef(ce, as, ctx, e, t)
            val qs1 = qs map { q => applySubst(ctx1.s, q) }
            val t1 = applySubst(ctx1.s, t)
            val fs = (as.values.toList flatMap { a => tv(applySubst(ctx1.s, a)) }).distinct
            val gs = tv(t1) diff fs
            val sc1 = sc.h match {
                case Forall(fi, _, _) => Qual.quantify(gs, Qual(qs1, t1), Some(fi))
                case _ => Qual.quantify(gs, Qual(qs1, t1))
            }
            val ps0 = ps map { applySubst(ctx1.s, _) }
            val ps1 = ps0 filter { case p => !entail(ce, qs1, p) }
            val (ds, rs) = split(ce, fs, ps1)
            if (sc != sc1) throw TIError("signature too general in " + prettyPrint(ex._1) + " ::\n\t\t(" + prettyPrint(sc) + ") it don't match\n\t\t(" + prettyPrint(sc1) + ")", e)
            else if (rs.nonEmpty) throw TIError("context too weak in " + prettyPrint(ex._1) + " :: " + prettyPrint(sc) + " it don't got " + (rs.map(prettyPrint).mkString(", ")), e)
            else (ctx1.setNodeType(e, Qual(qs1, t1)), ds)
    }

    /**
     * Infers the types of a group of implicitly type definitions.
     */
    def tiImpls(ce: ClassEnv, as: Assumps, ctx: Context, bs: List[Impl]): (Assumps, Context, List[IsIn]) = {
        val ts = bs map { _ => newTVar(Star) }
        val is = bs map { _._1 }
        val scs = ts map { t => Qual(Nil, t) }
        val as1 = as ++ (is zip scs)
        val es = bs map { _._2 }
        val (ctx1, ps) = ctx.flatMap(es zip ts) { case (ctx, (e, t)) => tiDef(ce, as1, ctx, e, t) }
        val ps1 = ps map { applySubst(ctx1.s, _) }
        val ts1 = ts map { applySubst(ctx1.s, _) }
        val fs = (as.values.toList map { applySubst(ctx1.s, _) } flatMap tv).distinct
        val vss = ts1 map tv
        val gs = vss.foldLeft(List.empty[TVar]) { (xs, ys) => xs ++ (ys filterNot { y => xs contains y }) } diff fs
        val (ds, rs) = split(ce, fs, ps1)
        val scs1 = ts1 map { t => Qual.quantify(gs, Qual(rs, t)) }
        val ctx2 = ctx1.foreach(es zip ts1) { case (ctx, (e, t)) => ctx.setNodeType(e, Qual(rs, t)) }
        (as ++ (is zip scs1), ctx2, ds)
    }

    /**
     * Type-check a list of binding groups and accumulate assumptions while running through the list.
     */
    def tiSeq[BG](ti: Infer[BG], ce: ClassEnv, as: Assumps, ctx: Context, bs: List[BG]): (Assumps, Context, List[IsIn]) = bs match {
        case List() => (as, ctx, Nil)
        case bs :: bss =>
            val (as1, ctx1, ps) = ti(ce, as, ctx, bs)
            val (as2, ctx2, qs) = tiSeq(ti, ce, as1, ctx1, bss)
            (as2, ctx2, ps ++ qs)
    }

    /**
     * Infers the types of the definitions within a binding group.
     */
    def tiBindGroup(ce: ClassEnv, as: Assumps, ctx: Context, bg: BindGroup): (Assumps, Context, List[IsIn]) = {
        val (es, iss) = bg
        val as1 = as ++ (es collect { case (v, sc, _) => v -> sc })
        val (as2, ctx1, ps) = tiSeq(tiImpls, ce, as1, ctx, iss)
        val (ctx2, qs) = ctx1.flatMap(es) { case (ctx, e) => tiExpl(ce, as2, ctx, e) }
        (as2, ctx2, ps ++ qs)
    }

    def tiProgram(ce: ClassEnv, as: Assumps, bgs: List[BindGroup]): (Assumps, Subst, ExprTypeMap) = {
        val (as1, ctx, ps) = tiSeq(tiBindGroup, ce, as, Context(nullSubst, Map.empty), bgs)
        val rs = reduce(ce, ps map { p => applySubst(ctx.s, p) })
        rs foreach { r => if (!entail(ce, Nil, r)) throw TIInternalError("No instance exists for " + prettyPrint(r)) }
        (as1 mapValues { a => applySubst(ctx.s, a) }, ctx.s, ctx.ets)
    }
}