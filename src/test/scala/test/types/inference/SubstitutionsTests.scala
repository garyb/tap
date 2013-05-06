package test.types.inference

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.types._
import tap.types.Type._
import tap.types.kinds._
import tap.types.inference.Substitutions._
import tap.ModuleId
import tap.types.classes.{Qual, IsIn}
import language.reflectiveCalls

class SubstitutionsTests extends FlatSpec {

    behavior of "applySubst for Type"

    it should "do nothing when encountering a type var that is not in the substitutions" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val x = TVar("x", Star)
        applySubst(Map(a -> x), b) should be === b
    }

    it should "return the new type when encountering a type var is in the substitutions" in {
        val a = TVar("a", Star)
        val x = TVar("x", Star)
        applySubst(Map(a -> x), a) should be === x
    }

    it should "apply substitutions to type vars in a TAp" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val x = TVar("x", Star)
        val y = TVar("y", Star)
        applySubst(Map(a -> x, b -> y), TAp(a, b)) should be === TAp(x, y)
    }

    it should "apply substitutions to type vars in a Forall" in {
        val a = TVar("a", Star)
        val x = TVar("x", Star)
        applySubst(Map(a -> x), Forall(0, Nil, a)) should be === Forall(0, Nil, x)
    }

    it should "do nothing when encountering other types" in {
        val a = TVar("a", Star)
        val x = TVar("x", Star)
        val s = Map(a -> x)
        applySubst(s, TCon(ModuleId("Prelude", "Number"), Star)) should be === TCon(ModuleId("Prelude", "Number"), Star)
        applySubst(s, TGen(0, 0)) should be === TGen(0, 0)
    }

    //-------------------------------------------------------------------------

    "applySubst for IsIn" should "apply substitutions to the predicates list" in {
        val a = TVar("a", Star)
        val x = TVar("x", Star)
        val s = Map(a -> x)
        applySubst(s, IsIn(ModuleId("Test", "Test"), List(a))) should be === IsIn(ModuleId("Test", "Test"), List(x))
    }

    //-------------------------------------------------------------------------

    "applySubst for Qual[Type]" should "apply substitutions to the predicates list and type" in {
        val a = TVar("a", Star)
        val x = TVar("x", Star)
        val s = Map(a -> x)
        applySubst(s, Qual(List(IsIn(ModuleId("Test", "Test"), List(a))), a)) should be === Qual(List(IsIn(ModuleId("Test", "Test"), List(x))), x)
    }

    //-------------------------------------------------------------------------

    behavior of "tv for Type"

    it should "return a type variable" in {
        val a = TVar("a", Star)
        tv(a) should be === List(a)
    }

    it should "return all the type variables in a type application" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        tv(TAp(a, b)) should be === List(a, b)
    }

    it should "return all the unquantified type variables in a forall" in {
        val a = TVar("a", Star)
        tv(Forall(0, Nil, a)) should be === List(a)
        tv(Forall(0, List(Star), TAp(a, TGen(0, 0)))) should be === List(a)
    }

    it should "return nothing for other types" in {
        tv(TCon(ModuleId("Prelude", "String"), Star)) should be === Nil
        tv(TGen(0, 0)) should be === Nil
    }

    it should "return a list of distinct type variables" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        tv(Forall(0, List(Star), TAp(a, b fn a fn TGen(0, 0) fn b))) should be === List(a, b)
    }

    //-------------------------------------------------------------------------

    "tv for IsIn" should "return a list of distinct type variables from the types list" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        tv(IsIn(ModuleId("Test", "Test"), List(a, TAp(a, b)))) should be === List(a, b)
    }

    //-------------------------------------------------------------------------

    "tv for Qual[Type]" should "return a list of distinct type variables from the predicates list and type" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val c = TVar("c", Star)
        tv(Qual(List(IsIn(ModuleId("Test", "Test"), List(a, TAp(a, b)))), c fn c)) should be === List(a, b, c)
    }

    //-------------------------------------------------------------------------

    "@@" should "compose substitutions" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val c = TVar("c", Star)
        val x = TVar("x", Star)
        val y = TVar("y", Star)
        val q = TVar("q", Star)
        val s1 = Map(x -> tString, y -> q)
        val s2 = Map(a -> x, b -> TAp(y, c))
        val s3 = composeSubst(s1, s2)
        s3 should be === Map(a -> tString, b -> TAp(q, c), x -> tString, y -> q)
    }

    //-------------------------------------------------------------------------

    behavior of "merge"

    it should "return None if the substitutions have conflicting entrie" in {
        val a = TVar("a", Star)
        val x = TVar("x", Star)
        val y = TVar("y", Star)
        val s1 = Map(a -> x)
        val s2 = Map(a -> y)
        merge(s1, s2) should be === None
    }

    it should "return the combined substitution if the substitutions have no overlap" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val x = TVar("x", Star)
        val y = TVar("y", Star)
        val s1 = Map(a -> x)
        val s2 = Map(b -> y)
        merge(s1, s2) should be === Some(Map(a -> x, b -> y))
    }

    it should "return the combined substitution if the substitutions equivalent entries" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val c = TVar("c", Star)
        val x = TVar("x", Star)
        val y = TVar("y", Star)
        val z = TVar("z", Star)
        val s1 = Map(a -> x, c -> z)
        val s2 = Map(b -> y, c -> z)
        merge(s1, s2) should be === Some(Map(a -> x, b -> y, c -> z))
    }
 }
