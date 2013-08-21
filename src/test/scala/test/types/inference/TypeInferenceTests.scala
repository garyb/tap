package test.types.inference

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.types.inference.TypeInference._

import tap.types.Type._
import tap.types.Natives._
import tap.types._
import tap.types.kinds._
import tap.types.classes.{ClassEnvironments, IsIn, Qual}
import tap._
import tap.types.inference.{TIEnv, TIError, Substitutions, TIInternalError}
import tap.ir._
import tap.util.trace
import tap.util.PrettyPrint._
import language.reflectiveCalls

class TypeInferenceTests extends FlatSpec {

    val testCE = ClassEnvironments.nullEnv
    val testAs = Map[Id, Qual[Type]](
        ModuleId("Prelude", "show") -> Qual.quantify(List(TVar("a", Star)), Qual(List(IsIn(ModuleId("Prelude", "show"), List(TVar("a", Star)))), TVar("a", Star) fn tString))._2
    )
    val nullCtx = TIEnv.empty

    //-------------------------------------------------------------------------

    behavior of "freshInst for Type"

    it should "return the input if passed a non-Forall type" in {
        nullCtx.freshInst(tNumber)._2 should be === tNumber
        nullCtx.freshInst(tNumber fn tString)._2 should be === (tNumber fn tString)
        nullCtx.freshInst(TVar("a", Star) fn tString)._2 should be === (TVar("a", Star) fn tString)
        nullCtx.freshInst(TGen(0, 0) fn TGen(0, 0))._2 should be === (TGen(0, 0) fn TGen(0, 0))
    }

    it should "replace TGens in a Forall with new type variables" in {
        val result = nullCtx.freshInst(Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0)))._2
        result should be === (TVar("µ0", Star) fn TVar("µ0", Star))
    }

    it should "only replace TGens belonging to the current Forall with new type variables" in {
        val result = nullCtx.freshInst(Forall(0, List(Star), TGen(0, 0) fn TGen(1, 0)))._2
        result should be === (TVar("µ0", Star) fn TGen(1, 0))
    }

    //-------------------------------------------------------------------------

    behavior of "freshInst for Qual[Type]"

    it should "return the input if passed a non-Forall type" in {
        nullCtx.freshInst(Qual(Nil, tNumber))._2 should be === Qual(Nil, tNumber)
        nullCtx.freshInst(Qual(Nil, tNumber fn tString))._2 should be === Qual(Nil, tNumber fn tString)
        nullCtx.freshInst(Qual(Nil, TVar("a", Star) fn tString))._2 should be === Qual(Nil, TVar("a", Star) fn tString)
        nullCtx.freshInst(Qual(Nil, TGen(0, 0) fn TGen(0, 0)))._2 should be === Qual(Nil, TGen(0, 0) fn TGen(0, 0))

        nullCtx.freshInst(Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString))._2 should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString)
        nullCtx.freshInst(Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), TGen(0, 0) fn TGen(0, 0)))._2 should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), TGen(0, 0) fn TGen(0, 0))
    }

    it should "replace TGens in a Forall with new type variables, applying the same substitution to the predicates in the Qual" in {
        val result = nullCtx.freshInst(Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0))))._2
        val lastVar = TVar("µ0", Star)
        result should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(lastVar))), lastVar fn lastVar)
    }

    it should "only replace TGens belonging to the current Forall with new type variables" in {
        val result = nullCtx.freshInst(Qual(List(
            IsIn(ModuleId("Test", "Class"), List(TGen(0, 0))),
            IsIn(ModuleId("Test", "Class"), List(TGen(1, 0)))),
            Forall(0, List(Star), TGen(0, 0) fn Forall(1, List(Star), TGen(1, 0)))))._2
        val lastVar = TVar("µ0", Star)
        result should be === Qual(List(
            IsIn(ModuleId("Test", "Class"), List(lastVar)),
            IsIn(ModuleId("Test", "Class"), List(TGen(1, 0)))),
            lastVar fn Forall(1, List(Star), TGen(1, 0)))
    }

    //-------------------------------------------------------------------------

    behavior of "freshInstPartial for Type"

    it should "return the input if passed a non-Forall type" in {
        nullCtx.freshInstPartial(Nil, tNumber)._2 should be === tNumber
        nullCtx.freshInstPartial(Nil, tNumber fn tString)._2 should be === (tNumber fn tString)
        nullCtx.freshInstPartial(Nil, TVar("a", Star) fn tString)._2 should be === (TVar("a", Star) fn tString)
        nullCtx.freshInstPartial(Nil, TGen(0, 0) fn TGen(0, 0))._2 should be === (TGen(0, 0) fn TGen(0, 0))
    }

    it should "replace TGens in a Forall, first with the specified types, and after that with new type variables" in {
        val result = nullCtx.freshInstPartial(List(tString), Forall(0, List(Star, Star), TGen(0, 0) fn TGen(0, 1)))._2
        result should be === (tString fn TVar("µ0", Star))
    }

    it should "only replace TGens belonging to the current Forall with new type variables" in {
        val result = nullCtx.freshInstPartial(List(tString), Forall(0, List(Star, Star), TGen(0, 0) fn TGen(0, 1) fn TGen(1, 0)))._2
        result should be === (tString fn TVar("µ0", Star) fn TGen(1, 0))
    }

    it should "throw an error if too many types are provided" in {
        evaluating {
            nullCtx.freshInstPartial(List(tString, tNumber), Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0)))
        } should produce [TIInternalError]
    }

    //-------------------------------------------------------------------------

    behavior of "freshInstPartial for Qual[Type]"

    it should "return the input if passed a non-Forall type" in {
        nullCtx.freshInstPartial(Nil, Qual(Nil, tNumber))._2 should be === Qual(Nil, tNumber)
        nullCtx.freshInstPartial(Nil, Qual(Nil, tNumber fn tString))._2 should be === Qual(Nil, tNumber fn tString)
        nullCtx.freshInstPartial(Nil, Qual(Nil, TVar("a", Star) fn tString))._2 should be === Qual(Nil, TVar("a", Star) fn tString)
        nullCtx.freshInstPartial(Nil, Qual(Nil, TGen(0, 0) fn TGen(0, 0)))._2 should be === Qual(Nil, TGen(0, 0) fn TGen(0, 0))

        nullCtx.freshInstPartial(Nil, Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString))._2 should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TVar("a", Star)))), TVar("a", Star) fn tString)
        nullCtx.freshInstPartial(Nil, Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), TGen(0, 0) fn TGen(0, 0)))._2 should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), TGen(0, 0) fn TGen(0, 0))
    }

    it should "replace TGens in a Forall, first with the specified types, and after that with new type variables, applying the same substitution to the predicates in the Qual" in {
        val result = nullCtx.freshInstPartial(List(tString), Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), Forall(0, List(Star, Star), TGen(0, 0) fn TGen(0, 1))))._2
        val lastVar = TVar("µ0", Star)
        result should be === Qual(List(IsIn(ModuleId("Test", "Class"), List(tString))), tString fn lastVar)
    }

    it should "only replace TGens belonging to the current Forall with new type variables" in {
        val result = nullCtx.freshInstPartial(List(tString), Qual(List(
            IsIn(ModuleId("Test", "Class"), List(TGen(0, 0))),
            IsIn(ModuleId("Test", "Class"), List(TGen(1, 0)))),
            Forall(0, List(Star, Star), TGen(0, 0) fn TGen(0, 1) fn Forall(1, List(Star), TGen(1, 0)))))._2
        val lastVar = TVar("µ0", Star)
        result should be === Qual(List(
            IsIn(ModuleId("Test", "Class"), List(tString)),
            IsIn(ModuleId("Test", "Class"), List(TGen(1, 0)))),
            tString fn lastVar fn Forall(1, List(Star), TGen(1, 0)))
    }

    it should "throw an error if too many types are provided" in {
        evaluating {
            nullCtx.freshInstPartial(List(tString, tNumber), Qual(List(IsIn(ModuleId("Test", "Class"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0))))
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

    it should "replace the outermost forall TGens with type variables when inferring for ValueReadExprs" in {
        val ce = testCE
        val qt = Qual(List(IsIn(ModuleId("Data.Monoid", "Monoid"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0)))
        val as = testAs + (LocalId("testval") -> qt)
        val ctx0 = nullCtx
        val expr = ValueReadExpr(LocalId("testval"))
        val (ctx1, ps, t) = tiExpr(ce, as, ctx0, expr, Nil)
        val tv = TVar("µ0", Star)
        val qs = List(IsIn(ModuleId("Data.Monoid", "Monoid"), List(tv)))
        ctx1 should be === ctx0.setNodeType(expr, Qual(qs, tv)).copy(uniq = 1)
        ps should be === qs
        t should be === tv
    }

    it should "not replace inner forall TGens with type variables when inferring for ValueReadExprs" in {
        val ce = testCE
        val tv = TVar("a", Star)
        val qt = Qual(Nil, Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0)) fn tv)
        val as = testAs + (LocalId("testfn") -> qt)
        val ctx0 = nullCtx
        val expr = ValueReadExpr(LocalId("testfn"))
        val (ctx1, ps, t) = tiExpr(ce, as, ctx0, expr, Nil)
        ctx1 should be === ctx0.setNodeType(expr, qt)
        ps should be === qt.ps
        t should be === qt.h
    }

    it should "build type inference constraints for CastExprs" in {
        val ce = testCE
        val tv = TVar("a", Star)
        val qt = Qual(List(IsIn(ModuleId("Data.Monoid", "Monoid"), List(tv))), tv)
        val as = testAs + (LocalId("testval") -> qt)
        val ctx0 = nullCtx
        val expr0 = ValueReadExpr(LocalId("testval"))
        val expr1 = CastExpr(expr0, tNumber)
        val (ctx1, ps, t) = tiExpr(ce, as, ctx0, expr1, Nil)
        ctx1 should be === ctx0
                .unify(qt.h, tNumber, expr1)
                .setNodeType(expr0, qt)
                .setNodeType(expr1, qt)
        ps should be === qt.ps
        t should be === qt.h
    }

    it should "throw an error if a CastExpr is invalid" in {
        val ce = testCE
        val as = testAs
        val ctx0 = nullCtx
        val expr = CastExpr(StringExpr("oh no"), tNumber)
        evaluating {
            tiExpr(ce, as, ctx0, expr, Nil)
        } should produce [TIError]
    }

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

    it should "build type inference constraints for NumberExprs" in {
        val ce = testCE
        val as = testAs
        val ctx0 = nullCtx
        val expr = NumberExpr(0)
        val (ctx1, ps, t) = tiExpr(ce, as, ctx0, expr, Nil)
        ctx1 should be === ctx0.setNodeType(expr, tNumber)
        ps should be === Nil
        t should be === tNumber
    }

    ignore should "build type inference constraints for FunctionExprs" in {}
    ignore should "build type inference constraints for FunctionExprs with no arguments" in {}
    ignore should "build type inference constraints for NativeValueExprs" in {}

    it should "throw an error if the value in a RaiseErrorExpr does not unify with String" in {
        val ce = testCE
        val as = testAs
        val ctx0 = nullCtx
        val expr = RaiseErrorExpr(NumberExpr(0))
        evaluating {
            tiExpr(ce, as, ctx0, expr, Nil)
        } should produce [TIError]
    }

    it should "build type inference constraints for RaiseErrorExprs" in {
        val ce = testCE
        val as = testAs + (LocalId("msg") -> Qual(Nil, TVar("s", Star)))
        val ctx0 = nullCtx
        val expr0 = ValueReadExpr(ModuleId("Prelude", "show"))
        val expr1 = ValueReadExpr(LocalId("msg"))
        val expr2 = ApplyExpr(expr0, expr1)
        val expr3 = RaiseErrorExpr(expr2)
        val (ctx1, ps, t) = tiExpr(ce, as, ctx0, expr3, Nil)
        val tvShow = TVar("µ0", Star)
        val tvApply = TVar("µ1", Star)
        val tvResult = TVar("µ2", Star)
        ctx1 should be === ctx0
                .unify(TVar("s", Star), tvShow, expr2)
                .unify(tvApply, tString, expr3)
                .setNodeType(expr0, Qual(List(IsIn(ModuleId("Prelude", "show"), List(tvShow))), tvShow fn tString))
                .setNodeType(expr1, TVar("s", Star))
                .setNodeType(expr2, tvApply)
                .setNodeType(expr3, tvResult)
                .copy(uniq = 3)
        ps should be === List(IsIn(ModuleId("Prelude", "show"), List(tvShow)))
        t should be === tvResult
    }

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
