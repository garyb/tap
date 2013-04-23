package test

import org.scalatest.matchers.{MatchResult, Matcher}
import tap.ir.ApplyExpr
import tap.ir.BlockExpr
import tap.ir.NativeValueExpr
import tap.ir.NumberExpr
import tap.ir.StringExpr
import tap.ir._

trait TapNodeEquality {

    private def test(left: TapNode, right: TapNode): Boolean = (left, right) match {
        case (BlockExpr(xs), BlockExpr(ys)) => (xs zip ys) forall { case (x, y) => test(x, y) }
        case (ApplyExpr(f1, a), ApplyExpr(f2, b)) => test(f1, f2) && test(a, b)
        case (MatchExpr(e1, cs1), MatchExpr(e2, cs2)) => test(e1, e2) && ((cs1 zip cs2) forall { case (c, d) => test(c, d) })
        case (LetExpr(id1, v1, s1), LetExpr(id2, v2, s2)) => id1 == id2 && test(v1, v2) && test(s1, s2)
        case (ValueReadExpr(id1), ValueReadExpr(id2)) => id1 == id2
        case (CastExpr(v1, t1), CastExpr(v2, t2)) => test(v1, v2) && t1 == t2
        case (RaiseErrorExpr(e1), RaiseErrorExpr(e2)) => test(e1, e2)
        case (Argument(id1), Argument(id2)) => id1 == id2
        case (MatchCase(v1, None, e1), MatchCase(v2, None, e2)) => test(v1, v2) && test(e1, e2)
        case (MatchCase(v1, Some(g1), e1), MatchCase(v2, Some(g2), e2)) => test(v1, v2) && test(g1, g2) && test(e1, e2)
        case (UnapplyNode(id1, xs), UnapplyNode(id2, ys)) => id1 == id2 && ((xs zip ys) forall { case (x, y) => test(x, y) })
        case (BindNode(id1, None), BindNode(id2, None)) => id1 == id2
        case (BindNode(id1, Some(v1)), BindNode(id2, Some(v2))) => id1 == id2 && test(v1, v2)
        case (StringExpr(x), StringExpr(y)) => x == y
        case (NumberExpr(x), NumberExpr(y)) => x == y
        case (FunctionExpr(a1, b1), FunctionExpr(a2, b2)) => test(a1, a2) && test(b1, b2)
        case (NativeValueExpr(x, t), NativeValueExpr(y, u)) => x == y && t == u
        case (x, y) => x == y
    }

    /**
     * Unfortunately normal case class equality fails for TapExprs, because they intentionally
     * have overrides enforcing non-equality so a map of their types can be constructed. There
     * is probably a better way.
     */
    class NodeEqualityMatcher(right: TapNode) extends Matcher[TapNode] {

        def apply(left: TapNode) =
            MatchResult(
                test(left, right),
                left.toString + " was not like " + right.toString,
                left.toString + " was like " + right.toString
            )
    }

    class NodeEqualityMatchers(rights: Iterable[TapNode]) extends Matcher[Iterable[TapNode]] {

        def apply(lefts: Iterable[TapNode]) =
            MatchResult(
                (lefts zip rights) forall { case (l, r) => test(l, r) },
                lefts.toString + " was not like " + rights.toString,
                lefts.toString + " was like " + rights.toString
            )
    }

    def equal(right: TapNode) = new NodeEqualityMatcher(right)
    def equal(rights: Iterable[TapNode]) = new NodeEqualityMatchers(rights)
}
