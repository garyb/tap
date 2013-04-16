package test.ir

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.ir.TapNodeUtil._
import tap.ir._
import tap.types.Type
import tap.{InstId, LocalId, ModuleId}
import test.TapNodeEquality

class TapNodeUtilTests extends FlatSpec with TapNodeEquality {

	// ------------------------------------------------------------------------

	behavior of "getAppliedFunc"

	it should "travel down the left side of a chain of nested ApplyExprs until a value is found" in {

		getAppliedFunc(ApplyExpr(ValueReadExpr(LocalId("fn")), NumberExpr(1))) should
			equal(ValueReadExpr(LocalId("fn")))

		getAppliedFunc(ApplyExpr(ApplyExpr(ValueReadExpr(LocalId("fn")), NumberExpr(1)), NumberExpr(2))) should
			equal(ValueReadExpr(LocalId("fn")))
	}

	// ------------------------------------------------------------------------

	behavior of "getApplyArgs"

	it should "accumulate the values from the right side in a chain of nested ApplyExprs" in {
		getApplyArgs(ApplyExpr(ApplyExpr(ValueReadExpr(LocalId("fn")), NumberExpr(1)), NumberExpr(2))) should
			equal(List(NumberExpr(1), NumberExpr(2)))
	}

	// ------------------------------------------------------------------------

	behavior of "makeApply"

	it should "return the original function when provided an empty list" in {
		val fn = ValueReadExpr(LocalId("fn"))
		makeApply(fn, List.empty) should equal(fn)
	}

	it should "create a chain of ApplyExprs when provided with a list of arguments" in {
		val fn = ValueReadExpr(LocalId("fn"))
		makeApply(fn, List(NumberExpr(1), NumberExpr(2))) should
			equal(ApplyExpr(ApplyExpr(fn, NumberExpr(1)), NumberExpr(2)))
	}

	// ------------------------------------------------------------------------

	behavior of "makeFunc"

	it should "return the intended function body when provided an empty list" in {
		val body = NumberExpr(1)
		makeFunc(List.empty, body) should equal(body)
	}

	it should "create a chain of FunctionExpr when provided with a list of arguments" in {
		val body = NumberExpr(1)

		makeFunc(List(Argument("a"), Argument("b")), body) should
			equal(FunctionExpr(Argument("a"), FunctionExpr(Argument("b"), body)))

		makeFunc(List(NoArgument), body) should
			equal(FunctionExpr(NoArgument, body))
	}

	// ------------------------------------------------------------------------

	behavior of "makeBlock"

	it should "return the default value when the list is empty" in {
		val default = NumberExpr(1)
		makeBlock(List.empty, default) should equal(default)
	}

	it should "return the first item in the list when the list only has one item" in {
		val default = NumberExpr(1)
		val item = NumberExpr(2)
		makeBlock(List(item), default) should equal(item)
	}

	it should "create a BlockExpr containing the provided values" in {
		val default = NumberExpr(1)
		val items = List(NumberExpr(2), NumberExpr(3), NumberExpr(4))
		makeBlock(items, default) should equal(BlockExpr(items))
	}

	// ------------------------------------------------------------------------

	behavior of "findDependencies"

	it should "find all references in an UnapplyNode" in {
		val x = ModuleId("Test", "x")
		val y = ModuleId("Test", "y")
		findDependencies(UnapplyNode(x, List(UnapplyNode(y, List.empty)))) should be === Set(x, y)
	}

	it should "find all references inside a BindNode" in {
		val x = ModuleId("Test", "x")
		findDependencies(BindNode("a", Some(UnapplyNode(x, List.empty)))) should be === Set(x)
	}

	it should "find all references inside a MatchCase" in {
		val x = ModuleId("Test", "x")
		val y = ModuleId("Test", "y")
		val z = ModuleId("Test", "z")
		findDependencies(MatchCase(UnapplyNode(x, List.empty), Some(ValueReadExpr(y)), ValueReadExpr(z))) should be === Set(x, y, z)
	}

	it should "find nothing in a ValueReadExpr local value reference" in {
		findDependencies(ValueReadExpr(LocalId("x"))) should be === Set.empty
	}

	it should "find references in a ValueReadExpr module value reference" in {
		val x = ModuleId("Test", "x")
		findDependencies(ValueReadExpr(x)) should be === Set(x)
	}

	it should "find references in a ValueReadExpr instance value reference" in {
		val x = InstId("Test", ModuleId("Test", "Test"), List.empty, "x")
		findDependencies(ValueReadExpr(x)) should be === Set(x)
	}

	it should "find all references inside a BlockExpr" in {
		val x = ModuleId("Test", "x")
		val y = ModuleId("Test", "y")
		findDependencies(BlockExpr(List(ValueReadExpr(x), ValueReadExpr(y)))) should be === Set(x, y)
	}

	it should "find all references inside an ApplyExpr" in {
		val x = ModuleId("Test", "x")
		val y = ModuleId("Test", "y")
		findDependencies(ApplyExpr(ValueReadExpr(x), ValueReadExpr(y))) should be === Set(x, y)
	}

	it should "find all references inside a MatchExpr" in {
		val x = ModuleId("Test", "x")
		val y = ModuleId("Test", "y")
		val z = ModuleId("Test", "z")
		val w = ModuleId("Test", "w")
		findDependencies(MatchExpr(ValueReadExpr(x), List(MatchCase(UnapplyNode(y, List.empty), Some(ValueReadExpr(z)), ValueReadExpr(w))))) should be === Set(x, y, z, w)
	}

	it should "find all references inside a LetExpr" in {
		val x = ModuleId("Test", "x")
		val y = ModuleId("Test", "y")
		findDependencies(LetExpr("x1", ValueReadExpr(x), ValueReadExpr(y))) should be === Set(x,y )
	}

	it should "find all references inside a FunctionExpr" in {
		val x = ModuleId("Test", "x")
		findDependencies(FunctionExpr(Argument("a"), ValueReadExpr(x))) should be === Set(x)
	}

	it should "find all references inside a CastExpr" in {
		val x = ModuleId("Test", "x")
		findDependencies(CastExpr(ValueReadExpr(x), Type.tString)) should be === Set(x)
	}

	it should "find all references inside a ErrorExpr" in {
		val x = ModuleId("Test", "x")
		findDependencies(RaiseErrorExpr(ValueReadExpr(x))) should be === Set(x)
	}

	// ------------------------------------------------------------------------

	behavior of "findImmediateDependencies"

	it should "ignore references within a FunctionExpr" in {
		val x = ModuleId("Test", "x")
		findImmediateDependencies(FunctionExpr(NoArgument, ValueReadExpr(x))) should be === Set.empty
	}

	it should "find all other references" in {
		// TODO: findImmediateDependencies can be refined extensively, but for
		// now this is the intended behaviour. see the comment on the function
		// definition for more details.

		val x = ModuleId("Test", "x")
		val y = ModuleId("Test", "y")
		val z = ModuleId("Test", "z")
		val w = ModuleId("Test", "w")

		findDependencies(ValueReadExpr(x)) should be === Set(x)
		findDependencies(BlockExpr(List(ValueReadExpr(x), ValueReadExpr(y)))) should be === Set(x, y)
		findDependencies(ApplyExpr(ValueReadExpr(x), ValueReadExpr(y))) should be === Set(x, y)
		findDependencies(MatchExpr(ValueReadExpr(x), List(MatchCase(UnapplyNode(y, List.empty), Some(ValueReadExpr(z)), ValueReadExpr(w))))) should be === Set(x, y, z, w)
		findDependencies(LetExpr("x1", ValueReadExpr(x), ValueReadExpr(y))) should be === Set(x,y )
		findDependencies(FunctionExpr(Argument("a"), ValueReadExpr(x))) should be === Set(x)
		findDependencies(CastExpr(ValueReadExpr(x), Type.tString)) should be === Set(x)
	}

	// ------------------------------------------------------------------------

	behavior of "findLocalIds"

	it should "return the id from an Argument" in {
		findLocalIds(Argument("x")) should be === Set("x")
	}

	it should "return all ids inside a BindNode" in {
		findLocalIds(BindNode("x", Some(BindNode("y", None)))) should be === Set("x", "y")
	}

	it should "return all ids inside a UnapplyNode" in {
		findLocalIds(UnapplyNode(ModuleId("Test", "x"), List(BindNode("y", None), BindNode("z", None)))) should be === Set("y", "z")
	}

	it should "return all ids inside a MatchCase" in {
		findLocalIds(MatchCase(BindNode("x", None), Some(ValueReadExpr(LocalId("y"))), ValueReadExpr(LocalId("z")))) should be === Set("x", "y", "z")
	}

	it should "return the id from a ValueReadExpr when the reference is to a LocalId" in {
		findLocalIds(ValueReadExpr(LocalId("x"))) should be === Set("x")
	}

	it should "return nothing from a ValueReadExpr when the reference is to a ModuleId" in {
		findLocalIds(ValueReadExpr(ModuleId("Test", "x"))) should be === Set.empty
	}

	it should "return nothing from a ValueReadExpr when the reference is to a InstId" in {
		findLocalIds(ValueReadExpr(InstId("Test", ModuleId("Test", "Test"), List.empty, "x"))) should be === Set.empty
	}

	it should "return all ids from a LetExpr" in {
		findLocalIds(LetExpr("x", ValueReadExpr(LocalId("y")), ValueReadExpr(LocalId("z")))) should be === Set("x", "y", "z")
	}

	it should "return all ids inside a BlockExpr" in {
		findLocalIds(BlockExpr(List(ValueReadExpr(LocalId("x")), ValueReadExpr(LocalId("y"))))) should be === Set("x", "y")
	}

	it should "return all ids inside a ApplyExpr" in {
		findLocalIds(ApplyExpr(ValueReadExpr(LocalId("x")), ValueReadExpr(LocalId("y")))) should be === Set("x", "y")
	}

	it should "return all ids inside a MatchExpr" in {
		findLocalIds(MatchExpr(ValueReadExpr(LocalId("w")), List(MatchCase(BindNode("x", None), Some(ValueReadExpr(LocalId("y"))), ValueReadExpr(LocalId("z")))))) should be === Set("w", "x", "y", "z")
	}

	it should "return all ids inside a FunctionExpr" in {
		findLocalIds(FunctionExpr(Argument("x"), ValueReadExpr(LocalId("y")))) should be === Set("x", "y")
	}

	it should "return all ids inside a ErrorExpr" in {
		findLocalIds(RaiseErrorExpr(ValueReadExpr(LocalId("x")))) should be === Set("x")
	}

	it should "return all ids inside a CastExpr" in {
		findLocalIds(CastExpr(ValueReadExpr(LocalId("x")), Type.tString)) should be === Set("x")
	}
}
