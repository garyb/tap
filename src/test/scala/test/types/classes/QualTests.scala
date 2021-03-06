package test.types.classes

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.ModuleId
import tap.types._
import tap.types.inference.Substitutions.nullSubst
import tap.types.classes.Qual._
import tap.types.classes.{IsIn, Qual}
import tap.types.kinds.Star
import language.reflectiveCalls
import tap.types.inference.TIEnv

class QualTests extends FlatSpec {

    val nullEnv = TIEnv.empty

    //-------------------------------------------------------------------------

    behavior of "tv"

    it should "return a list of distinct type variables from the predicates list and type" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val c = TVar("c", Star)
        Qual.tv(Qual(List(IsIn(ModuleId("Test", "Test"), List(a, TAp(a, b)))), c fn c)) should be === List(a, b, c)
    }

    //-------------------------------------------------------------------------

    behavior of "inst"

    it should "throw an error when the number of provided types does not match the number of TGens" in {
        val a = TVar("a", Star)
        val sc = Qual(Nil, Forall(0, List(Star), TGen(0, 0) fn (TGen(0, 1) fn TGen(0, 0))))
        evaluating { inst(sc.h, List.empty, sc) } should produce [IndexOutOfBoundsException]
        evaluating { inst(sc.h, List(a), sc) } should produce [IndexOutOfBoundsException]
    }

    it should "replace TGens in the specified Forall with the provided types" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val sc = Qual(Nil, Forall(0, List(Star), TGen(0, 0) fn (a fn TGen(0, 0))))
        inst(sc.h, List(b), sc) should be === Qual(Nil, b fn (a fn b))
    }

    it should "only replace TGens belonging to the specified Forall" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val sc = Qual(Nil, Forall(0, List(Star), TGen(0, 0) fn Forall(1, List(Star), a fn TGen(1, 0))))
        inst(sc.h, List(b), sc) should be === Qual(Nil, b fn Forall(1, List(Star), a fn TGen(1, 0)))
    }

    it should "replace TGens in the predicates list" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val ps = IsIn(ModuleId("Test", "TC"), List(TGen(0, 0)))
        val sc = Qual(List(ps), Forall(0, List(Star), TGen(0, 0) fn (a fn TGen(0, 0))))
        inst(sc.h, List(b), sc) should be === Qual(List(IsIn(ModuleId("Test", "TC"), List(b))), b fn (a fn b))
    }

    //-------------------------------------------------------------------------

    behavior of "inst without specifiying the current forall"

    it should "use the current type as the forall" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val sc = Qual(Nil, Forall(0, List(Star), TGen(0, 0) fn Forall(1, List(Star), a fn TGen(1, 0))))
        inst(List(b), sc) should be === Qual(Nil, b fn Forall(1, List(Star), a fn TGen(1, 0)))
    }

    //-------------------------------------------------------------------------

    behavior of "quantify"

    it should "return the input when none of the provided type variables are in the type" in {
        val a = TVar("a", Star)
        quantify(nullEnv, List.empty, Qual(Nil, a fn a))._3 should be === Qual(Nil, a fn a)
    }

    it should "return substitutions for the variables replaced with TGens" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val s = quantify(nullEnv, List(a), Qual(Nil, a fn (b fn a)))._2
        s should be === nullSubst + (a -> TGen(0, 0))
    }

    it should "only quantify type variables in the provided list" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val sc = quantify(nullEnv, List(a), Qual(Nil, a fn (b fn a)))._3
        sc should be === Qual(Nil, Forall(0, List(Star), TGen(0, 0) fn (b fn TGen(0, 0))))
    }

    it should "quantify type variables in the predicates list" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val sc = quantify(nullEnv, List(a), Qual(List(IsIn(ModuleId("Test", "TC"), List(a))), a fn (b fn a)))._3
        sc should be === Qual(List(IsIn(ModuleId("Test", "TC"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn (b fn TGen(0, 0))))
    }
 }
