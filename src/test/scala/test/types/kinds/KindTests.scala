package test.types.kinds

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.ModuleId
import tap.types._
import tap.types.Type._
import tap.types.kinds._
import tap.types.kinds.Kind._
import language.reflectiveCalls

class KindTests extends FlatSpec {

    behavior of "kind"

    it should "return a type variables's kind when given a TVar" in {
        kind(TVar("a", Star)) should be === Star
        kind(TVar("a", Kfun(Star, Star))) should be === Kfun(Star, Star)
    }

    it should "return a type constructor's kind when given a TCon" in {
        kind(TCon(ModuleId("Test", "A"), Star)) should be === Star
        kind(TCon(ModuleId("Test", "B"), Kfun(Star, Star))) should be === Kfun(Star, Star)
    }

    it should "return a type's kind when given a TVar" in {
        kind(TVar("a", Star)) should be === Star
        kind(TVar("a", Kfun(Star, Star))) should be === Kfun(Star, Star)
    }

    it should "return a type's kind when given a TCon" in {
        kind(TCon(ModuleId("Test", "A"), Star)) should be === Star
        kind(TCon(ModuleId("Test", "B"), Kfun(Star, Star))) should be === Kfun(Star, Star)
    }

    it should "return a type's kind when given a Forall" in {
        kind(Forall(0, List(Star), TCon(ModuleId("Test", "A"), Star))) should be === Star
        kind(Forall(1, List(Star), TCon(ModuleId("Test", "B"), Kfun(Star, Star)))) should be === Kfun(Star, Star)
    }

    it should "return a type's kind when given a TAp" in {
        val tEither = TCon(ModuleId("Test", "Either"), Kfun(Star, Kfun(Star, Star)))
        kind(TAp(tEither, tBool)) should be === Kfun(Star, Star)
        kind(TAp(TAp(tEither, tBool), tNumber)) should be === Star
    }

    it should "throw an error when given an invalid TAp" in {
        evaluating { kind(TAp(tBool, tBool)) } should produce [Error]
    }

    it should "throw an error when given a TGen" in {
        evaluating { kind(TGen(0, 0)) } should produce [Error]
        evaluating { kind(TAp(TGen(0, 0), tBool)) } should produce [Error]
    }

    behavior of "arity"

    it should "return a type's arity from its kind" in {
        arity(Star) should be === 0
        arity(Kfun(Star, Star)) should be === 1
        arity(Kfun(Star, Kfun(Star, Star))) should be === 2
        arity(Kfun(Star, Kfun(Star, Kfun(Star, Star)))) should be === 3
        arity(Kfun(Kfun(Star, Star), Star)) should be === 1
    }
}
