package tap.types.inference

import Substitutions._
import tap.ast.FilePositional
import tap.util.{trace, ContextOps}
import ContextOps._
import tap.ir._
import tap.types.Type._
import tap.types.Natives._
import tap.types._
import tap.types.classes.ClassEnvironments.ClassEnv
import tap.types.classes.IsIn.{reduce, entail, split}
import tap.types.classes._
import tap.types.kinds.{Kind, Star}
import tap.util.PrettyPrint._
import tap.{LocalId, Id}
import language.reflectiveCalls

object TypeInference {

    type Assumps = Map[Id, Qual[Type]]

    def toQual(ps: List[IsIn], t: Type): Qual[Type] = {
        val tvs = tv(t)
        val ps1 = ps filter { p => IsIn.tv(p) forall { tv => tvs contains tv } }
        Qual(ps1, t)
    }

    // -----------------------------------------------------------------------------------------------------------
    //  Type inference
    // -----------------------------------------------------------------------------------------------------------

    type ExprTypeMap = Map[TapNode, Qual[Type]]

    /**
     * Builds the constraints for inferring the type of an expression.
     */
    def tiExpr(ce: ClassEnv, as: Assumps, ctx: TIEnv, node: TapExpr, explArgs: List[Type]): (TIEnv, List[IsIn], Type) = node match {

        case BlockExpr(es) =>
            def traverse(ctx: TIEnv, ps: List[IsIn], es: Seq[TapExpr], result: Type): (TIEnv, List[IsIn], Type) = es match {
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
            val (ctx3, t) = ctx2.newTVar(Star)
            val ctx4 = ctx3.unify(te fn t, tf, node)
            (ctx4.setNodeType(node, t), ps ++ qs, t)

        case MatchExpr(e, branches) =>
            val (ctx1, ps, t) = tiExpr(ce, as, ctx, e, explArgs)
            val (ctx2, v) = ctx1.newTVar(Star)
            val (ctx3, qs) = ctx2.flatMap(branches) { case (ctx, cn) => tiBranch(ce, as, ctx, t, v, cn, explArgs) }
            (ctx3.setNodeType(node, v), ps ++ qs, v)

        case LetExpr(name, e, f) =>
            val (ctx1, t) = ctx.newTVar(Star)
            val as1 = as + (LocalId(name) -> Qual(Nil, t))
            val (ctx2, ps, et) = tiExpr(ce, as1, ctx1, e, explArgs)
            val ctx3 = ctx2.unify(t, et, node)
            val et1 = applySubst(ctx3.s, et)
            val as2 = as + (LocalId(name) -> toQual(ps, et1))
            val (ctx4, qs, ft) = tiExpr(ce, as2, ctx3, f, explArgs)
            (ctx4.setNodeType(node, ft), ps ++ qs, ft)

        case ValueReadExpr(i) =>
            val (ctx1, qt) = ctx.freshInst(as(i))
            (ctx1.setNodeType(node, qt), qt.ps, qt.h)

        case CastExpr(e, ct) =>
            val (ctx1, ps, et) = tiExpr(ce, as, ctx, e, explArgs)
            val ctx2 = ctx1.unify(et, ct, node)
            (ctx2.setNodeType(node, Qual(ps, et)), ps, et)

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
            val (ctx1, at) = ctx.newTVar(Star)
            val as1 = as + (LocalId(arg) -> Qual(Nil, at))
            val (ctx2, ps, rt) = tiExpr(ce, as1, ctx1, b, explArgs)
            val bt = at fn rt
            (ctx2.setNodeType(node, bt).setNodeType(argNode, at), ps, bt)

        case RaiseErrorExpr(e) =>
            val (ctx1, ps, te) = tiExpr(ce, as, ctx, e, explArgs)
            val ctx2 = ctx1.unify(te, tString, e)
            val (ctx3, t) = ctx2.newTVar(Star)
            (ctx3.setNodeType(node, t), ps, t)
    }

    def tiBranch(ce: ClassEnv, as: Assumps, ctx: TIEnv, t: Type, v: Type, node: MatchCase, explArgs: List[Type]): (TIEnv, List[IsIn]) = node match {

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

    def tiPattern(as: Assumps, ctx: TIEnv, node: PatternNode): (Assumps, TIEnv, List[IsIn], Type) = node match {

        case UnapplyNode(name, args) =>
            val (as1, ctx1, ps, ts) = tiPatterns(as, ctx, args)
            val (ctx2, t1) = ctx1.newTVar(Star)
            val (ctx3, Qual(qs, t)) = ctx2.freshInst(as1(name))
            val t2 = ts.foldRight(t1) { _ fn _ }
            val ctx4 = ctx3.unify(t, t2, node)
            (as1, ctx4.setNodeType(node, t1), ps ++ qs, t1)

        case BindNode(name, None) =>
            val (ctx1, v) = ctx.newTVar(Star)
            (as + (LocalId(name) -> Qual(Nil, v)), ctx1.setNodeType(node, v), Nil, v)

        case BindNode(name, Some(pat)) =>
            val (as1, ctx1, ps, t) = tiPattern(as, ctx, pat)
            (as1 + (LocalId(name) -> Qual(Nil, t)), ctx1.setNodeType(node, t), ps, t)

        case _: StringExpr => (as, ctx.setNodeType(node, tString), Nil, tString)
        case _: NumberExpr => (as, ctx.setNodeType(node, tNumber), Nil, tNumber)

        case WildcardValueExpr =>
            val (ctx1, v) = ctx.newTVar(Star)
            (as, ctx1.setNodeType(node, v), Nil, v)
    }

    def tiPatterns(as: Assumps, ctx: TIEnv, pats: List[PatternNode]): (Assumps, TIEnv, List[IsIn], List[Type]) =
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
    type Infer[E] = (ClassEnv, Assumps, TIEnv, E) => (Assumps, TIEnv, List[IsIn])

    /**
     * Type infers a definition and checks it against an expected type. Used by both implicit and explicitly typed
     * definitions, in the case of implicit definitions t will be a TVar so the check will always pass. Returns a list
     * of predicates that were accumulated from e.
     */
    def tiDef(ce: ClassEnv, as: Assumps, ctx: TIEnv, e: TapExpr, t: Type): (TIEnv, List[IsIn]) = {
        val (ctx1, qs, t1) = tiExpr(ce, as, ctx, e, getFunctionTypeArgs(t))
        val ctx2 = ctx1.unify(t, t1, e)
        (ctx2.setNodeType(e, Qual(qs, t)), qs)
    }

    /**
     * Type checks an explicitly typed definition.
     */
    def tiExpl(ce: ClassEnv, as: Assumps, ctx: TIEnv, ex: Expl): (TIEnv, List[IsIn]) = ex match {
        case (id, sc, e) =>
            val (ctx1, Qual(qs, t)) = ctx.freshInst(sc)
            val (ctx2, ps) = tiDef(ce, as, ctx1, e, t)
            val qs1 = qs map { q => applySubst(ctx2.s, q) }
            val t1 = applySubst(ctx2.s, t)
            val fs = (as.values.toList flatMap { a => Qual.tv(applySubst(ctx2.s, a)) }).distinct
            val gs = tv(t1) diff fs
            val (ctx3, _, sc1) = sc.h match {
                case Forall(fi, _, _) => Qual.quantify(ctx2, gs, Qual(qs1, t1), Some(fi))
                case _ => Qual.quantify(ctx2, gs, Qual(qs1, t1))
            }
            val ps0 = ps map { applySubst(ctx3.s, _) }
            val ps1 = ps0 filter { case p => !entail(ce, qs1, p) }
            val (ds, rs) = split(ce, fs, ps1)
            if (sc != sc1) throw TIError("signature too general in " + prettyPrint(ex._1) + " ::\n\t\t(" + prettyPrint(sc) + ") it don't match\n\t\t(" + prettyPrint(sc1) + ")", e)
            else if (rs.nonEmpty) throw TIError("context too weak in " + prettyPrint(ex._1) + " :: " + prettyPrint(sc) + " it don't got " + rs.map(prettyPrint).mkString(", "), e)
            else (ctx3.setNodeType(e, Qual(qs1, t1)), ds)
    }

    /**
     * Infers the types of a group of implicitly type definitions.
     */
    def tiImpls(ce: ClassEnv, as: Assumps, ctx: TIEnv, bs: List[Impl]): (Assumps, TIEnv, List[IsIn]) = {
        val (ctx1, ts) = ctx.map(bs) { case (ctx, _) => ctx.newTVar(Star) }
        val is = bs map { _._1 }
        val scs = ts map { t => Qual(Nil, t) }
        val as1 = as ++ (is zip scs)
        val es = bs map { _._2 }
        val (ctx2, ps) = ctx1.flatMap(es zip ts) { case (ctx, (e, t)) => tiDef(ce, as1, ctx, e, t) }
        val ps1 = ps map { applySubst(ctx2.s, _) }
        val ts1 = ts map { applySubst(ctx2.s, _) }
        val fs = (as.values.toList map { applySubst(ctx2.s, _) } flatMap Qual.tv).distinct
        val vss = ts1 map tv
        val gs = vss.foldLeft(List.empty[TVar]) { (xs, ys) => xs ++ (ys filterNot { y => xs contains y }) } diff fs
        val (ds, rs) = split(ce, fs, ps1)
        val (ctx3, scs1) = ctx2.map(ts1) { (ctx, t) =>
            val (ctx1, _, t1) = Qual.quantify(ctx, gs, Qual(rs, t))
            (ctx1, t1)
        }
        val ctx4 = ctx3.foreach(es zip ts1) { case (ctx, (e, t)) => ctx.setNodeType(e, Qual(rs, t)) }
        (as ++ (is zip scs1), ctx4, ds)
    }

    /**
     * Type-check a list of items and accumulate assumptions while running through the list.
     */
    def tiSeq[E](ti: Infer[E], ce: ClassEnv, as: Assumps, ctx: TIEnv, bs: List[E]): (Assumps, TIEnv, List[IsIn]) = bs match {
        case List() => (as, ctx, Nil)
        case bs :: bss =>
            val (as1, ctx1, ps) = ti(ce, as, ctx, bs)
            val (as2, ctx2, qs) = tiSeq(ti, ce, as1, ctx1, bss)
            (as2, ctx2, ps ++ qs)
    }

    /**
     * Infers the types of the definitions within a binding group.
     */
    def tiBindGroup(ce: ClassEnv, as: Assumps, ctx: TIEnv, bg: BindGroup): (Assumps, TIEnv, List[IsIn]) = {
        val (es, iss) = bg
        val as1 = as ++ (es collect { case (v, sc, _) => v -> sc })
        val (as2, ctx1, ps) = tiSeq(tiImpls, ce, as1, ctx, iss)
        val (ctx2, qs) = ctx1.flatMap(es) { case (ctx, e) => tiExpl(ce, as2, ctx, e) }
        (as2, ctx2, ps ++ qs)
    }

    def tiProgram(ce: ClassEnv, as: Assumps, ctx: TIEnv, bgs: List[BindGroup]): (Assumps, TIEnv) = {
        val (as1, ctx1, ps) = tiSeq(tiBindGroup, ce, as, ctx, bgs)
        val rs = reduce(ce, ps map { p => applySubst(ctx1.s, p) })
        rs foreach { r => if (!entail(ce, Nil, r)) throw TIInternalError("No instance exists for " + prettyPrint(r)) }
        (as1 mapValues { a => applySubst(ctx1.s, a) }, ctx1)
    }
}