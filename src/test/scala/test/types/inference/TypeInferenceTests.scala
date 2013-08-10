package test.types.inference

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.types.inference.TypeInference._

import tap.types.Type._
import tap.types._
import tap.types.kinds._
import tap.types.classes.{ClassEnvironments, IsIn, Qual}
import tap._
import tap.types.inference.{Substitutions, TIInternalError}
import tap.ir._
import tap.util.trace

class TypeInferenceTests extends FlatSpec {

    val testCE = ClassEnvironments.nullEnv
    val testAs = Map.empty[Id, Qual[Type]]
    val nullCtx = Context(Substitutions.nullSubst, Map.empty[TapNode, Qual[Type]])

    //-------------------------------------------------------------------------

    behavior of "freshInst for Type"

    it should "return the input if passed a non-Forall type" in {
        freshInst(tNumber) should be === tNumber
        freshInst(tNumber fn tString) should be === (tNumber fn tString)
        freshInst(TVar("a", Star) fn tString) should be === (TVar("a", Star) fn tString)
        freshInst(TGen(0, 0) fn TGen(0, 0)) should be === (TGen(0, 0) fn TGen(0, 0))
    }

    it should "replace TGens in a Forall with new type variables" in {
        val result = freshInst(Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0)))
        val lastVarName = "µ" + tvId
        result should be === (TVar(lastVarName, Star) fn TVar(lastVarName, Star))
    }

    it should "only replace TGens belonging to the current Forall with new type variables" in {
        val result = freshInst(Forall(0, List(Star), TGen(0, 0) fn TGen(1, 0)))
        val lastVarName = "µ" + tvId
        result should be === (TVar(lastVarName, Star) fn TGen(1, 0))
    }

    //-------------------------------------------------------------------------

    behavior of "freshInst for Qual[Type]"

    it should "return the input if passed a non-Forall type" in {
        freshInst(Qual(Nil, tNumber)) should be === Qual(Nil, tNumber)
        freshInst(Qual(Nil, tNumber fn tString)) should be === Qual(Nil, tNumber fn tString)
        freshInst(Qual(Nil, TVar("a", Star) fn tString)) should be === Qual(Nil, TVar("a", Star) fn tString)
        freshInst(Qual(Nil, TGen(0, 0) fn TGen(0, 0))) should be === Qual(Nil, TGen(0, 0) fn TGen(0, 0))

        freshInst(Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString)) should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString)
        freshInst(Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), TGen(0, 0) fn TGen(0, 0))) should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), TGen(0, 0) fn TGen(0, 0))
    }

    it should "replace TGens in a Forall with new type variables, applying the same substitution to the predicates in the Qual" in {
        val result = freshInst(Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0))))
        val lastVar = TVar("µ" + tvId, Star)
        result should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(lastVar))), lastVar fn lastVar)
    }

    it should "only replace TGens belonging to the current Forall with new type variables" in {
        val result = freshInst(Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn TGen(1, 0))))
        val lastVar = TVar("µ" + tvId, Star)
        result should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(lastVar))), lastVar fn TGen(1, 0))
    }

    //-------------------------------------------------------------------------

    behavior of "freshInstPartial for Type"

    it should "return the input if passed a non-Forall type" in {
        freshInstPartial(Nil, tNumber) should be === tNumber
        freshInstPartial(Nil, tNumber fn tString) should be === (tNumber fn tString)
        freshInstPartial(Nil, TVar("a", Star) fn tString) should be === (TVar("a", Star) fn tString)
        freshInstPartial(Nil, TGen(0, 0) fn TGen(0, 0)) should be === (TGen(0, 0) fn TGen(0, 0))
    }

    it should "replace TGens in a Forall, first with the specified types, and after that with new type variables" in {
        val result = freshInstPartial(List(tString), Forall(0, List(Star, Star), TGen(0, 0) fn TGen(0, 1)))
        val lastVarName = "µ" + tvId
        result should be === (tString fn TVar(lastVarName, Star))
    }

    it should "only replace TGens belonging to the current Forall with new type variables" in {
        val result = freshInstPartial(List(tString), Forall(0, List(Star, Star), TGen(0, 0) fn TGen(0, 1) fn TGen(1, 0)))
        val lastVarName = "µ" + tvId
        result should be === (tString fn TVar(lastVarName, Star) fn TGen(1, 0))
    }

    it should "throw an error if too many types are provided" in {
        evaluating {
            freshInstPartial(List(tString, tNumber), Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0)))
        } should produce [TIInternalError]
    }

    //-------------------------------------------------------------------------

    behavior of "freshInstPartial for Qual[Type]"

    it should "return the input if passed a non-Forall type" in {
        freshInstPartial(Nil, Qual(Nil, tNumber)) should be === Qual(Nil, tNumber)
        freshInstPartial(Nil, Qual(Nil, tNumber fn tString)) should be === Qual(Nil, tNumber fn tString)
        freshInstPartial(Nil, Qual(Nil, TVar("a", Star) fn tString)) should be === Qual(Nil, TVar("a", Star) fn tString)
        freshInstPartial(Nil, Qual(Nil, TGen(0, 0) fn TGen(0, 0))) should be === Qual(Nil, TGen(0, 0) fn TGen(0, 0))

        freshInstPartial(Nil, Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString)) should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString)
        freshInstPartial(Nil, Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), TGen(0, 0) fn TGen(0, 0))) should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), TGen(0, 0) fn TGen(0, 0))
    }

    it should "replace TGens in a Forall, first with the specified types, and after that with new type variables, applying the same substitution to the predicates in the Qual" in {
        val result = freshInstPartial(List(tString), Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), Forall(0, List(Star, Star), TGen(0, 0) fn TGen(0, 1))))
        val lastVar = TVar("µ" + tvId, Star)
        result should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(tString))), tString fn lastVar)
    }

    it should "only replace TGens belonging to the current Forall with new type variables" in {
        val result = freshInstPartial(List(tString), Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), Forall(0, List(Star, Star), TGen(0, 0) fn TGen(0, 1) fn TGen(1, 0))))
        val lastVar = TVar("µ" + tvId, Star)
        result should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(tString))), tString fn lastVar fn TGen(1, 0))
    }

    it should "throw an error if too many types are provided" in {
        evaluating {
            freshInstPartial(List(tString, tNumber), Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0))))
        } should produce [TIInternalError]
    }

    //-------------------------------------------------------------------------

    behavior of "toQual"

    it should "construct a qualified type using the specified predicates and type" in {
        val preds = List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star))))
        val t = TVar("a", Star) fn tString
        toQual(preds, t) should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString)
    }

    it should "omit any predicates from the Qual that are for type variables not appearing in the type" in {
        val preds = List(IsIn(ModuleId("Test", "ClassX"), List(TVar("a", Star))), IsIn(ModuleId("Test", "ClassY"), List(TVar("b", Star))))
        val t = TVar("b", Star) fn tString
        toQual(preds, t) should be === Qual(List(IsIn(ModuleId("Test", "ClassY"), List(TVar("b", Star)))), TVar("b", Star) fn tString)
    }

    //-------------------------------------------------------------------------

    behavior of "tiExpr"
    ignore should "build type inference constraints for BlockExprs" in {}
    ignore should "build type inference constraints for ApplyExprs" in {}
    ignore should "build type inference constraints for MatchExprs" in {}
    ignore should "build type inference constraints for LetExprs" in {}

    it should "build type inference constraints for ValueReadExprs" in {
        val ce = testCE
        val tv = TVar("a", Star)
        val qt = Qual(List(IsIn(ModuleId("Data.Monoid", "Monoid"), List(tv))), tv)
        val as = testAs + (LocalId("testval") -> qt)
        val ctx0 = nullCtx
        val expr = ValueReadExpr(LocalId("testval"))
        val (ctx1, ps, t) = tiExpr(ce, as, ctx0, expr, Nil)
        ctx1 should be === ctx0.setNodeType(expr, qt)
        ps should be === qt.ps
        t should be === qt.h
    }

    ignore should "build type inference constraints for CastExprs" in {}

    it should "build type inference constraints for StringExprs" in {
        val ce = testCE
        val as = testAs
        val ctx0 = nullCtx
        val expr = StringExpr("hwaet!")
        val (ctx1, ps, t) = tiExpr(ce, as, ctx0, expr, Nil)
        ctx1 should be === ctx0.setNodeType(expr, tString)
        ps should be === Nil
        t should be === tString
    }

    ignore should "build type inference constraints for NumberExprs" in {}
    ignore should "build type inference constraints for FunctionExprs" in {}
    ignore should "build type inference constraints for FunctionExprs with no arguments" in {}
    ignore should "build type inference constraints for NativeValueExprs" in {}
    ignore should "build type inference constraints for RaiseErrorExprs" in {}

    //-------------------------------------------------------------------------

    behavior of "tiBranch"
    ignore should "build type inference constraints for MatchCases" in {}
    ignore should "build type inference constraints for MatchCases with guards" in {}

    //-------------------------------------------------------------------------

    behavior of "tiPattern"
    ignore should "build type inference constraints for UnapplyNodes" in {}
    ignore should "build type inference constraints for BindNodes with no sub-pattern" in {}
    ignore should "build type inference constraints for BindNodes with a sub-pattern" in {}
    ignore should "build type inference constraints for StringExprs" in {}
    ignore should "build type inference constraints for NumberExprs" in {}
    ignore should "build type inference constraints for WildcardValueExprs" in {}

    //-------------------------------------------------------------------------

    behavior of "tiPatterns"
    ignore should "accumulate the type inference constraints for a list of PatternNodes" in {}

    //-------------------------------------------------------------------------

    behavior of "tiDef"
    ignore should "add a type inference constraint checking a definition against a type" in {}

    //-------------------------------------------------------------------------

    behavior of "tiExpl"
    ignore should "check the type of an explicitly typed definition" in {}
    ignore should "throw an error if the explicit type signature is too general for the inferred type" in {}
    ignore should "throw an error if there are inferred predicates missing from the explicit type" in {}

    //-------------------------------------------------------------------------

    behavior of "tiImpls"
    ignore should "infer the types of each item in a group of implicitly typed definitions" in {}

    //-------------------------------------------------------------------------

    behavior of "tiSeq"
    ignore should "sequentially run a type inference function on a list of items, accumulating the result" in {}

    //-------------------------------------------------------------------------

    behavior of "tiBindGroup"
    ignore should "type check every definition in a binding group" in {}

    //-------------------------------------------------------------------------

    behavior of "tiProgram"
    ignore should "type check every definition in a list of binding groups" in {}
    ignore should "throw an error if an non-existant instance is required somewhere" in {}

 }
