package tap.ir

import TapNodeUtil._
import tap._
import tap.ast._
import tap.util.IDUtil
import tap.verifier.errors._
import tap.types.{Type, TCon}

// --------------------------------------------------------------------------------------------------------------------

sealed trait TapNode extends FilePositional {
    override val hashCode = new Object().hashCode
    override def equals(other: Any) = hashCode == other.hashCode && super.equals(other)
}
sealed trait TapExpr extends TapNode
sealed trait ExprNode extends TapExpr
sealed trait ExprLeaf extends TapExpr

// ---[ expressions ]--------------------------------------------------------------------------------------------------

case class BlockExpr(children: List[TapExpr]) extends ExprNode
case class ApplyExpr(func: TapExpr, arg: TapExpr) extends ExprNode
case class MatchExpr(expr: TapExpr, cases: List[MatchCase]) extends ExprNode
case class LetExpr(name: String, value: TapExpr, scope: TapExpr) extends ExprNode
case class ValueReadExpr(ref: Id) extends ExprLeaf
case class CastExpr(value: TapExpr, t: Type) extends ExprNode
case class RaiseErrorExpr(value: TapExpr) extends ExprNode

// ---[ match cases & patterns ]---------------------------------------------------------------------------------------

case class MatchCase(value: PatternNode, guard: Option[TapExpr], expr: TapExpr) extends TapNode

sealed trait PatternNode extends TapNode
case class BindNode(name: String, value: Option[PatternNode]) extends PatternNode
case class UnapplyNode(dcon: Id, args: List[PatternNode]) extends PatternNode
case object WildcardValueExpr extends PatternNode

// ---[ values ]-------------------------------------------------------------------------------------------------------

case class StringExpr(value: String) extends ExprLeaf with PatternNode
case class NumberExpr(value: Double) extends ExprLeaf with PatternNode
case class FunctionExpr(arg: ArgumentDef, body: TapExpr) extends ExprNode
case class NativeValueExpr(id: Id, t: Type) extends ExprLeaf

// ---[ argument declarations ]----------------------------------------------------------------------------------------

sealed trait ArgumentDef extends TapNode { def name: String }
case class Argument(name: String) extends ArgumentDef
case object NoArgument extends ArgumentDef { val name = "_" }

// --------------------------------------------------------------------------------------------------------------------

object TapNode {

    case class ResolveState(dcons: Map[String, ModuleId],
                            defs: Map[String, ModuleId],
                            locals: Set[String],
                            tconLookup: Map[String, ModuleId],
                            tcons: Map[ModuleId, TCon]) {

        def addLocal(name: String): ResolveState = ResolveState(dcons, defs, locals + name, tconLookup, tcons)
    }

    private def vUnit = ValueReadExpr(ModuleId("Prelude", "Unit"))

    def fromAST(expr: ASTExpr, state: ResolveState, nativeContext: Id, natives: Map[Id, Type]): TapExpr = expr match {
        case ASTNativeValue =>
            natives.get(nativeContext) match {
                case Some(ttype) => NativeValueExpr(nativeContext, ttype).setPos(expr.pos)
                case None => throw InvalidNativeError(nativeContext, expr)
            }

        case _ => fromAST(expr, state)
    }

    def fromAST(expr: ASTExpr, state: ResolveState): TapExpr = (expr match {

        case ASTBlock(exprs) =>
            def loop(xs: List[ASTExpr], state: ResolveState): List[TapExpr] = xs match {
                case List() => List.empty
                case ASTLet(name, value) :: xs =>
                    val state1 = state.addLocal(name)
                    List(LetExpr(name, fromAST(value, state1), makeBlock(loop(xs, state1), ValueReadExpr(LocalId(name)))))
                case x :: xs => fromAST(x, state) :: loop(xs, state)
            }
            makeBlock(loop(exprs, state), vUnit)

        case ASTMatch(expr, Seq()) => throw NoCaseError(expr)
        case ASTMatch(expr, cases) => MatchExpr(fromAST(expr, state), cases map { c => fromCaseAST(c, state) })

        case ASTApply(func, List()) => ApplyExpr(fromAST(func, state), vUnit)

        case ASTApply(func, args) if args contains ASTWildcardValue =>
            val resids = state.locals
            val (_, tmpNames) = args.filter { a => a == ASTWildcardValue }.foldRight((0, List.empty[String])) {
                case (_, (n, result)) => (n + 1, IDUtil.makeAlphabeticId(n, resids) :: result)
            }
            val state1 = tmpNames.foldLeft(state) { (state, t) => state.addLocal(t) }
            val body = args.foldLeft((0, fromAST(func, state1): TapExpr)) {
                case ((n, result), ASTWildcardValue) => (n + 1, ApplyExpr(result, ValueReadExpr(LocalId(tmpNames(n)))))
                case ((n, result), arg) => (n, ApplyExpr(result, fromAST(arg, state1)))
            }._2
            makeFunc(tmpNames map { a => Argument(a) }, body)

        case ASTApply(func, args) => makeApply(fromAST(func, state), args map { a => fromAST(a, state) })

        case ASTValueRead(name) if state.locals contains name => ValueReadExpr(LocalId(name))
        case ASTValueRead(name) if state.defs contains name => ValueReadExpr(state.defs(name))
        case ASTValueRead(name) if state.dcons contains name => ValueReadExpr(state.dcons(name))
        case ASTValueRead(name) => throw MissingDefinitionError(name, expr)
        case ASTLet(name, value) => LetExpr(name, fromAST(value, state.addLocal(name)), ValueReadExpr(LocalId(name)))

        case ASTFunction(List(), body) => FunctionExpr(NoArgument, fromAST(body, state))
        case ASTFunction(args, body) =>
            val state1 = args.foldLeft(state) { (state, arg) => state.addLocal(arg) }
            makeFunc(args map { a => Argument(a) }, fromAST(body, state1))

        case ASTRaiseError(expr) => RaiseErrorExpr(fromAST(expr, state))
        case ASTCast(expr, ttype) =>
            val e = fromAST(expr, state)
            val t = ASTUtil.getType(state.tconLookup, state.tcons, Map.empty, ttype match {
                case t: ASTForall => t
                case t => ASTForall(ASTUtil.findTypeVars(t).toList, t)
            })._2
            CastExpr(e, t)

        case ASTString(value) => StringExpr(value)
        case ASTNumber(value) => NumberExpr(value)
        case ASTWildcardValue => throw IllegalUnderscoreError(expr)
        case ASTNativeValue => throw IllegalNativeError(expr)

    }).setFilePosFrom(expr)

    def fromCaseAST(cnode: ASTCaseBranch, state: ResolveState): MatchCase = (cnode match {
        case ASTCaseBranch(value, guard, expr) =>
            val (pat, state1, _) = fromCaseValueAST(value, state, Set.empty)
            MatchCase(pat, guard map { g => fromAST(g, state1) }, fromAST(expr, state1))
    }).setFilePosFrom(cnode)

    def fromCaseValueAST(bnode: ASTPattern, state: ResolveState, patVars: Set[String]): (PatternNode, ResolveState, Set[String]) = {
        val (pat, state1, patVars1) = (bnode match {

            case ASTValueRead(name) =>
                if (patVars contains name) throw DuplicatePatternBind(name, bnode)
                (BindNode(name, None), state.addLocal(name), patVars + name)

            case ASTBind(name, value) =>
                val (pat, state1, patVars1) = fromCaseValueAST(value, state, patVars)
                if (patVars1 contains name) throw DuplicatePatternBind(name, bnode)
                (BindNode(name, Some(pat)), state1.addLocal(name), patVars + name)

            case ASTUnapply(name, args) if state.dcons contains name =>
                val (pats, state1, patVars1) = args.foldRight((List.empty[PatternNode], state, patVars)) {
                    case (v, (pats, state, patVars)) =>
                        val (pat, state1, patVars1) = fromCaseValueAST(v, state, patVars)
                        (pat :: pats, state1, patVars1)
                }
                (UnapplyNode(state.dcons(name), pats), state1, patVars1)

            case ASTUnapply(name, _) => throw MissingDataConstructorError(name, bnode)
            case ASTWildcardValue => (WildcardValueExpr, state, patVars)
            case ASTString(value) => (StringExpr(value), state, patVars)
            case ASTNumber(value) => (NumberExpr(value), state, patVars)

        })
        (pat.setFilePosFrom(bnode), state1, patVars1)
    }
}