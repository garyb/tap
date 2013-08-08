package test.types.inference

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.types.inference.TypeInference._

import tap.types.Type._
import tap.types._
import tap.types.kinds._

class TypeInferenceTests extends FlatSpec {

    behavior of "freshInst for Type"

    it should "return the input if passed a non-Forall type" in {
        freshInst(tNumber) should be === tNumber
        freshInst(tNumber fn tString) should be === (tNumber fn tString)
        freshInst(TVar("a", Star) fn tString) should be === (TVar("a", Star) fn tString)
        freshInst(TGen(0, 0) fn TGen(0, 0)) should be === (TGen(0, 0) fn TGen(0, 0))
    }

    ignore should "replace TGens in a Forall with new type variables" in {}

    //-------------------------------------------------------------------------

    behavior of "freshInst for Qual[Type]"
    ignore should "return the input if passed a non-Forall type" in {}
    ignore should "replace TGens in a Forall with new type variables, applying the same substitution to the predicates in the Qual" in {}

    //-------------------------------------------------------------------------

    behavior of "freshInstPartial for Type"
    ignore should "return the input if passed a non-Forall type" in {}
    ignore should "replace TGens in a Forall, first with the specified types, and after that with new type variables" in {}

    //-------------------------------------------------------------------------

    behavior of "freshInstPartial for Qual[Type]"
    ignore should "return the input if passed a non-Forall type" in {}
    ignore should "replace TGens in a Forall, first with the specified types, and after that with new type variables, applying the same substitution to the predicates in the Qual" in {}

    //-------------------------------------------------------------------------

    behavior of "toQual"
    ignore should "construct a qualified type using the specified predicates and type" in {}
    ignore should "omit any predicates from the Qual that are for type variables not appearing in the type" in {}

    //-------------------------------------------------------------------------

    behavior of "tiExpr"
    ignore should "build type inference constraints for BlockExprs" in {}
    ignore should "build type inference constraints for ApplyExprs" in {}
    ignore should "build type inference constraints for MatchExprs" in {}
    ignore should "build type inference constraints for LetExprs" in {}
    ignore should "build type inference constraints for ValueReadExprs" in {}
    ignore should "build type inference constraints for CastExprs" in {}
    ignore should "build type inference constraints for StringExprs" in {}
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
    ignore should "do some things" in {}

    //-------------------------------------------------------------------------

    behavior of "tiExpl"
    ignore should "do some things" in {}

    //-------------------------------------------------------------------------

    behavior of "tiImpls"
    ignore should "do some things" in {}

    //-------------------------------------------------------------------------

    behavior of "tiSeq"
    ignore should "do some things" in {}

    //-------------------------------------------------------------------------

    behavior of "tiBindGroup"
    ignore should "do some things" in {}

    //-------------------------------------------------------------------------

    behavior of "tiProgram"
    ignore should "do some things" in {}

 }
