package test.ir

import org.scalatest.matchers.ShouldMatchers._
import org.scalatest.{GivenWhenThen, FlatSpec}
import tap.ast._
import tap.ir.TapNode._
import tap.ir._
import tap.types.{TCon, Type}
import tap.verifier.errors._
import tap.{Id, LocalId, ModuleId}
import test.TapNodeEquality

class TapNodeTests extends FlatSpec with TapNodeEquality with GivenWhenThen {

    val nullState = new ResolveState(Map.empty, Map.empty, Set.empty, Map.empty, Map.empty)

    // ------------------------------------------------------------------------

    behavior of "fromCaseValueAST for ASTWildcardValue"

    it should "return a WildcardValueExpr for the referenced value" in {
        val (x, _, _) = fromCaseValueAST(ASTWildcardValue, nullState, Set.empty)
        x should equal(WildcardValueExpr)
    }

    it should "return an unmodified state" in {
        val (_, x, _) = fromCaseValueAST(ASTWildcardValue, nullState, Set.empty)
        x should be === nullState
    }

    it should "return an unmodified list of bound variables" in {
        val (_, _, x) = fromCaseValueAST(ASTWildcardValue, nullState, Set.empty)
        x should be === Set.empty
    }

    // ------------------------------------------------------------------------

    behavior of "fromCaseValueAST for ASTString"

    it should "return a StringExpr for the referenced value" in {
        val (x, _, _) = fromCaseValueAST(ASTString("test"), nullState, Set.empty)
        x should equal(StringExpr("test"))
    }

    it should "return an unmodified state" in {
        val (_, x, _) = fromCaseValueAST(ASTString("test"), nullState, Set.empty)
        x should be === nullState
    }

    it should "return an unmodified list of bound variables" in {
        val (_, _, x) = fromCaseValueAST(ASTString("test"), nullState, Set.empty)
        x should be === Set.empty
    }

    // ------------------------------------------------------------------------

    behavior of "fromCaseValueAST for ASTNumber"

    it should "return a NumberExpr for the referenced value" in {
        val (x, _, _) = fromCaseValueAST(ASTNumber(42), nullState, Set.empty)
        x should equal(NumberExpr(42))
    }

    it should "return an unmodified state" in {
        val (_, x, _) = fromCaseValueAST(ASTNumber(42), nullState, Set.empty)
        x should be === nullState
    }

    it should "return an unmodified list of bound variables" in {
        val (_, _, x) = fromCaseValueAST(ASTNumber(42), nullState, Set.empty)
        x should be === Set.empty
    }

    // ------------------------------------------------------------------------

    behavior of "fromCaseValueAST for ASTValueRead"

    it should "throw an error if the id is already present in the current set of pattern variables" in {
        evaluating {
            fromCaseValueAST(ASTValueRead("a"), nullState, Set("a"))
        } should produce [DuplicatePatternBind]
    }

    it should "return a BindNode for the referenced value" in {
        val (x, _, _) = fromCaseValueAST(ASTValueRead("a"), nullState, Set.empty)
        x should equal(BindNode("a", None))
    }

    it should "return a state containing the binding" in {
        val (_, x, _) = fromCaseValueAST(ASTValueRead("a"), nullState, Set.empty)
        x should be === nullState.addLocal("a")
    }

    it should "add the name of the bound variable to the set of pattern variables" in {
        val (_, _, x) = fromCaseValueAST(ASTValueRead("a"), nullState, Set.empty)
        x should be === Set("a")
    }

    // ------------------------------------------------------------------------

    behavior of "fromCaseValueAST for ASTCaseBind"

    it should "throw an error if the id is already present in the current set of pattern variables" in {
        evaluating {
            fromCaseValueAST(ASTValueRead("a"), nullState, Set("a"))
        } should produce [DuplicatePatternBind]
    }

    it should "return a BindNode for the referenced value" in {
        val (x, _, _) = fromCaseValueAST(ASTBind("a", ASTWildcardValue), nullState, Set.empty)
        x should equal(BindNode("a", Some(WildcardValueExpr)))
    }

    it should "return a state containing the binding" in {
        val (_, x, _) = fromCaseValueAST(ASTBind("a", ASTWildcardValue), nullState, Set.empty)
        x should be === nullState.addLocal("a")
    }

    it should "add the name of the bound variable to the set of pattern variables" in {
        val (_, _, x) = fromCaseValueAST(ASTBind("a", ASTWildcardValue), nullState, Set.empty)
        x should be === Set("a")
    }

    // ------------------------------------------------------------------------

    behavior of "fromCaseValueAST for ASTUnapply"

    it should "throw an error if the referenced data constructor is not in the resolve state" in {
        evaluating {
            fromCaseValueAST(ASTUnapply("Tuple2", List(ASTValueRead("a"), ASTValueRead("b"))), nullState, Set.empty)
        } should produce [MissingDataConstructorError]
    }

    it should "throw an error if there are duplicate variable bindings in the destructure" in {
        evaluating {
            val state = ResolveState(Map("Tuple2" -> ModuleId("Prelude.Tuple", "Tuple2")), Map.empty, Set.empty, Map.empty, Map.empty)
            fromCaseValueAST(ASTUnapply("Tuple2", List(ASTValueRead("a"), ASTValueRead("a"))), state, Set.empty)
        } should produce [DuplicatePatternBind]
    }

    it should "return an UnapplyNode for the referenced value" in {
        val state = ResolveState(Map("Tuple2" -> ModuleId("Prelude.Tuple", "Tuple2")), Map.empty, Set.empty, Map.empty, Map.empty)
        val (x, _, _) = fromCaseValueAST(ASTUnapply("Tuple2", List(ASTValueRead("a"), ASTValueRead("b"))), state, Set.empty)
        x should equal(UnapplyNode(ModuleId("Prelude.Tuple", "Tuple2"), List(BindNode("a", None), BindNode("b", None))))
    }

    it should "return a state containing any nested bound variables" in {
        val state = ResolveState(Map("Tuple2" -> ModuleId("Prelude.Tuple", "Tuple2")), Map.empty, Set.empty, Map.empty, Map.empty)
        val (_, x, _) = fromCaseValueAST(ASTUnapply("Tuple2", List(ASTValueRead("a"), ASTValueRead("b"))), state, Set.empty)
        x should be === state.addLocal("a").addLocal("b")
    }

    it should "add the name of any nested bound variables to the set of pattern variables" in {
        val state = ResolveState(Map("Tuple2" -> ModuleId("Prelude.Tuple", "Tuple2")), Map.empty, Set.empty, Map.empty, Map.empty)
        val (_, _, x) = fromCaseValueAST(ASTUnapply("Tuple2", List(ASTValueRead("a"), ASTValueRead("b"))), state, Set.empty)
        x should be === Set("a", "b")
    }

    // ------------------------------------------------------------------------

    behavior of "fromCaseAST"

    it should "return a corresponding ASTCaseBranch" in {

        fromCaseAST(ASTCaseBranch(ASTWildcardValue, None, ASTNumber(0)), nullState) should
            equal(MatchCase(WildcardValueExpr, None, NumberExpr(0)))

        fromCaseAST(ASTCaseBranch(ASTNumber(1), None, ASTNumber(1)), nullState) should
            equal(MatchCase(NumberExpr(1), None, NumberExpr(1)))

        When("the case has a guard")
        val x = ASTCaseBranch(ASTWildcardValue, Some(ASTValueRead("True")), ASTNumber(2))

        Then("the corresponding TapExpr for the guard should be produced too")
        val state = nullState.addLocal("True")
        fromCaseAST(x, state) should
                equal(MatchCase(WildcardValueExpr, Some(ValueReadExpr(LocalId("True"))), NumberExpr(2)))
    }

    it should "enable access to variables bound in the pattern" in {
        fromCaseAST(ASTCaseBranch(ASTValueRead("a"), None, ASTValueRead("a")), nullState) should
            equal(MatchCase(BindNode("a", None), None, ValueReadExpr(LocalId("a"))))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTString"

    it should "return a StringExpr with the same value" in {
        fromAST(ASTString(""), nullState) should equal(StringExpr(""))
        fromAST(ASTString("test"), nullState) should equal(StringExpr("test"))
        fromAST(ASTString("a\nstr"), nullState) should equal(StringExpr("a\nstr"))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTNumber"

    it should "return a NumberExpr with the same value" in {
        fromAST(ASTNumber(0), nullState) should equal(NumberExpr(0))
        fromAST(ASTNumber(10), nullState) should equal(NumberExpr(10))
        fromAST(ASTNumber(0xFFFFFF), nullState) should equal(NumberExpr(0xFFFFFF))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTWildcardValue"

    it should "throw an IllegalUnderscoreError" in {
        evaluating { fromAST(ASTWildcardValue, nullState) } should produce [IllegalUnderscoreError]
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTNativeValue"

    it should "throw an IllegalNativeError" in {
        evaluating { fromAST(ASTNativeValue, nullState) } should produce [IllegalNativeError]
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTNativeValue with native context"

    it should "return a NativeValueExpr if a native has been defined for the current context" in {
        val id = LocalId("Test")
        val natives = Map[Id, Type](LocalId("Test") -> Type.tUnit)
        fromAST(ASTNativeValue, nullState, id, natives) should equal(NativeValueExpr(id, Type.tUnit))
    }

    it should "throw an InvalidNativeError if no native has been defined for the current context" in {
        evaluating {
            fromAST(ASTNativeValue, nullState, LocalId("Nothing"), Map.empty)
        } should produce [InvalidNativeError]
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTValueRead"

    it should "throw an error if the value being read is not in scope" in {
        evaluating {
            fromAST(ASTValueRead("test"), nullState)
        } should produce [MissingDefinitionError]
    }

    it should "produce a corresponding ValueReadExpr if the value is a local def" in {
        val state = nullState.addLocal("test")
        fromAST(ASTValueRead("test"), state) should equal(ValueReadExpr(LocalId("test")))
    }

    it should "produce a corresponding ValueReadExpr if the value is a module def" in {
        val state = ResolveState(Map.empty, Map("test" -> ModuleId("Test", "test")), Set.empty, Map.empty, Map.empty)
        fromAST(ASTValueRead("test"), state) should equal(ValueReadExpr(ModuleId("Test", "test")))
    }

    it should "produce a corresponding ValueReadExpr if the value is a data constructor" in {
        val state = ResolveState(Map("test" -> ModuleId("Test", "test")), Map.empty, Set.empty, Map.empty, Map.empty)
        fromAST(ASTValueRead("test"), state) should equal(ValueReadExpr(ModuleId("Test", "test")))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTApply"

    it should "produce a corresponding ApplyExpr for a no-argument apply" in {
        val state = nullState.addLocal("fn")
        fromAST(ASTApply(ASTValueRead("fn"), List()), state) should
            equal(ApplyExpr(ValueReadExpr(LocalId("fn")), ValueReadExpr(ModuleId("Prelude", "Unit"))))
    }

    it should "produce a corresponding chain of ApplyExprs for an apply with args" in {
        val state = nullState.addLocal("fn")
        fromAST(ASTApply(ASTValueRead("fn"), List(ASTNumber(1), ASTNumber(2))), state) should
            equal(ApplyExpr(ApplyExpr(ValueReadExpr(LocalId("fn")), NumberExpr(1)), NumberExpr(2)))
    }

    it should "eta-abstract applications using wildcard placeholder arguments" in {
        val state = nullState.addLocal("fn")
        fromAST(ASTApply(ASTValueRead("fn"), List(ASTNumber(1), ASTWildcardValue, ASTWildcardValue, ASTNumber(4))), state) should
            equal(FunctionExpr(Argument("b"), FunctionExpr(Argument("a"), ApplyExpr(ApplyExpr(ApplyExpr(ApplyExpr(ValueReadExpr(LocalId("fn")), NumberExpr(1)), ValueReadExpr(LocalId("b"))), ValueReadExpr(LocalId("a"))), NumberExpr(4)))))

        When("introducing new arguments they should not conflict with existing local definitions")
        val state2 = state.addLocal("a").addLocal("b").addLocal("b1").addLocal("b2")
        fromAST(ASTApply(ASTValueRead("fn"), List(ASTWildcardValue, ASTWildcardValue, ASTNumber(2))), state2) should
            equal(FunctionExpr(Argument("b3"), FunctionExpr(Argument("a1"), ApplyExpr(ApplyExpr(ApplyExpr(ValueReadExpr(LocalId("fn")), ValueReadExpr(LocalId("b3"))), ValueReadExpr(LocalId("a1"))), NumberExpr(2)))))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTMatch"

    it should "throw an error if no cases are present" in {
        evaluating {
            fromAST(ASTMatch(ASTNumber(1), List.empty), nullState)
        } should produce [NoCaseError]
    }

    it should "produce a corresponding MatchExpr" in {
        fromAST(ASTMatch(ASTNumber(1), List(
            ASTCaseBranch(ASTWildcardValue, None, ASTNumber(0)),
            ASTCaseBranch(ASTNumber(1), None, ASTNumber(1)))
        ), nullState) should
            equal(MatchExpr(NumberExpr(1), List(
                MatchCase(WildcardValueExpr, None, NumberExpr(0)),
                MatchCase(NumberExpr(1), None, NumberExpr(1)))))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTLet"

    it should "produce a corresponding LetExpr" in {
        fromAST(ASTLet("a", ASTNumber(1)), nullState) should
            equal(LetExpr("a", NumberExpr(1), ValueReadExpr(LocalId("a"))))
    }

    it should "allow recursive self-reference" in {
        fromAST(ASTLet("a", ASTValueRead("a")), nullState) should
            equal(LetExpr("a", ValueReadExpr(LocalId("a")), ValueReadExpr(LocalId("a"))))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTBlock"

    it should "produce a corresponding BlockExpr" in {
        fromAST(ASTBlock(List(ASTNumber(1), ASTNumber(2), ASTNumber(3))), nullState) should
            equal(BlockExpr(List(NumberExpr(1), NumberExpr(2), NumberExpr(3))))
    }

    it should "handle let scope creation" in {
        fromAST(ASTBlock(List(ASTLet("x", ASTNumber(1)), ASTValueRead("x"))), nullState) should
            equal(LetExpr("x", NumberExpr(1), ValueReadExpr(LocalId("x"))))
    }

    it should "allow recursive self-reference if the block contains a let" in {
        fromAST(ASTBlock(List(ASTLet("x", ASTValueRead("x")), ASTNumber(1))), nullState) should
            equal(LetExpr("x", ValueReadExpr(LocalId("x")), NumberExpr(1)))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTFunction"

    it should "produce a corresponding FunctionExpr for a no-argument function" in {
        fromAST(ASTFunction(List(), ASTNumber(1)), nullState) should
            equal(FunctionExpr(NoArgument, NumberExpr(1)))
    }

    it should "produce a corresponding FunctionExpr for a function with arguments" in {
        fromAST(ASTFunction(List("x", "y"), ASTValueRead("y")), nullState) should
            equal(FunctionExpr(Argument("x"), FunctionExpr(Argument("y"), ValueReadExpr(LocalId("y")))))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTRaiseError"

    it should "produce a corresponding RaiseErrorExpr" in {
        fromAST(ASTRaiseError(ASTString("Some error.")), nullState) should
            equal(RaiseErrorExpr(StringExpr("Some error.")))
    }

    // ------------------------------------------------------------------------

    behavior of "fromAST for ASTCast"

    it should "produce a corresponding CastExpr" in {
        val state = ResolveState(Map.empty, Map.empty, Set("a"), Map("String" -> ModuleId("Prelude", "String")), Map(ModuleId("Prelude", "String") -> Type.tString.asInstanceOf[TCon]))
        fromAST(ASTCast(ASTValueRead("a"), ASTTypeCon("String")), state) should
            equal(CastExpr(ValueReadExpr(LocalId("a")), Type.tString))
    }
}
