package test.util

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.util.ContextOps._
import language.reflectiveCalls

class ContextOpTests extends FlatSpec {

	case class Context(values: List[Int] = Nil)

	// ------------------------------------------------------------------------

	behavior of "map over lists"

	it should "maintain identity" in {
		val ctx = new Context()
		val items = List(1, 2, 3)
		val (ctx1, items1) = ctx.map(items) { case (ctx, x) => (ctx, x) }
		ctx1 should be === ctx
		items1 should be === items
	}

	it should "accumulate the new context in right-to-left order" in {
		val ctx = new Context()
		val items = List(1, 2, 3)
		val (ctx1, _) = ctx.map(items) { case (ctx, x) => (Context(x :: ctx.values), x) }
		ctx1 should be === Context(List(1, 2, 3))
	}

	// ------------------------------------------------------------------------

	behavior of "map over maps"

	it should "maintain identity" in {
		val ctx = new Context()
		val items = Map(1 -> "A", 2 -> "B", 3 -> "C")
		val (ctx1, items1) = ctx.map(items) { case (ctx, x) => (ctx, x) }
		ctx1 should be === ctx
		items1 should be === items
	}

	it should "accumulate the new context in right-to-left order" in {
		val ctx = new Context()
		val items = Map(1 -> "A", 2 -> "B", 3 -> "C")
		val (ctx1, _) = ctx.map(items) { case (ctx, x @ (k, v)) => (Context(k :: ctx.values), x) }
		ctx1 should be === Context(List(1, 2, 3))
	}

	// ------------------------------------------------------------------------

	behavior of "flatMap"

	it should "maintain the order of the input lists" in {
		val ctx = new Context()
		val items = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
		val (_, items1) = ctx.flatMap(items) { case (ctx, x) => (ctx, x) }
		items1 should be === List(1, 2, 3, 4, 5, 6, 7, 8, 9)
	}

	it should "accumulate the new context in right-to-left order" in {
		val ctx = new Context()
		val items = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
		val (ctx1, _) = ctx.flatMap(items) { case (ctx, xs) => (Context(xs ++ ctx.values), xs) }
		ctx1 should be === Context(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
	}

	// ------------------------------------------------------------------------

	behavior of "foreach"

	it should "accumulate the new context in right-to-left order" in {
		val ctx = new Context()
		val items = List(1, 2, 3)
		val ctx1 = ctx.foreach(items) { case (ctx, x) => Context(x :: ctx.values) }
		ctx1 should be === Context(List(1, 2, 3))
	}
}
