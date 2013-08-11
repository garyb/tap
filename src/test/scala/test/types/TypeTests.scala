package test.types

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.ModuleId
import tap.types._
import tap.types.Type._
import tap.types.kinds._
import tap.types.inference.Substitutions.nullSubst
import java.lang.IllegalArgumentException
import language.reflectiveCalls

class TypeTests extends FlatSpec {

    behavior of "getTConID"

    it should "return the ID when the input is a TCon" in {
        val id = ModuleId("Test", "Test")
        getTConID(TCon(id, Star)) should be === id
    }

    it should "extract a TCon from the left hand side of a TAp" in {
        val id = ModuleId("Test", "Test")
        getTConID(TAp(TCon(id, Star), tString)) should be === id
    }

    it should "extract a TCon from the left hand side of a chain of TAps" in {
        val id = ModuleId("Test", "Test")
        getTConID(TAp(TAp(TCon(id, Star), tString), tString)) should be === id
    }

    it should "throw an error when passed any other type" in {
        evaluating { getTConID(TVar("a", Star)) } should produce [IllegalArgumentException]
        evaluating { getTConID(TAp(TVar("a", Star), tString)) } should produce [IllegalArgumentException]
        evaluating { getTConID(TGen(0, 0)) } should produce [IllegalArgumentException]
        evaluating { getTConID(TAp(TGen(0, 0), tString)) } should produce [IllegalArgumentException]
        evaluating { getTConID(Forall(0, List.empty, tString)) } should produce [IllegalArgumentException]
    }

    //-------------------------------------------------------------------------

    behavior of "isFuncType"

    it should "return true for a type that is an application of tArrow" in {
        isFuncType(TAp(tArrow, tString)) should be (true)
        isFuncType(TAp(TAp(tArrow, tString), tNumber)) should be (true)
    }

    it should "return false for other applied types" in {
        isFuncType(TAp(TVar("a", Star), tString)) should be (false)
        isFuncType(TAp(TGen(0, 0), tString)) should be (false)
        isFuncType(TAp(Forall(0, List.empty, tList), tString)) should be (false)
    }

    it should "return false for any non-applied type" in {
        isFuncType(tArrow) should be (false)
        isFuncType(TVar("a", Star)) should be (false)
        isFuncType(TGen(0, 0)) should be (false)
        isFuncType(Forall(0, List.empty, tList)) should be (false)
    }

    //-------------------------------------------------------------------------

    behavior of "getFunctionTypes"

    it should "extract the argument and return types from a function type" in {
        val t1 = tNumber fn tString
        getFunctionTypes(t1) should be === List(tNumber, tString)

        val t2 = tNumber fn (tString fn tUnit)
        getFunctionTypes(t2) should be === List(tNumber, tString, tUnit)
    }

    it should "return the input when it is a non-function type" in {
        getFunctionTypes(tString) should be === List(tString)
    }

    //-------------------------------------------------------------------------

    behavior of "getFunctionTypeArgs"

    it should "extract the argument and types from a function type" in {
        val t1 = tNumber fn tString
        getFunctionTypeArgs(t1) should be === List(tNumber)

        val t2 = tNumber fn (tString fn tUnit)
        getFunctionTypeArgs(t2) should be === List(tNumber, tString)
    }

    it should "return an empty list when the input is a non-function type" in {
        getFunctionTypeArgs(tString) should be === List.empty
    }

    //-------------------------------------------------------------------------

    behavior of "getFunctionArity"

    it should "return the correct arity for a function type" in {
        val t1 = tNumber fn tString
        getFunctionArity(t1) should be (1)

        val t2 = tNumber fn (tString fn tUnit)
        getFunctionArity(t2) should be (2)
    }

    it should "return 0 for non-function types" in {
        getFunctionArity(tString) should be (0)
    }

    //-------------------------------------------------------------------------

    behavior of "makeFunctionType"

    it should "produce the desired type" in {
        makeFunctionType(List(tNumber, tString, tUnit)) should be === (tNumber fn (tString fn tUnit))
    }

    //-------------------------------------------------------------------------

    behavior of "quantify"

    it should "return the input when none of the provided type variables are in the type" in {
        val a = TVar("a", Star)
        quantify(List.empty, a fn a) should be === (nullSubst, a fn a)
    }

    it should "only quantify type variables in the provided list" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val sc = quantify(List(a), a fn (b fn a))._2
        val fi = lastForallId
        sc should be === Forall(fi, List(Star), TGen(fi, 0) fn (b fn TGen(fi, 0)))
    }

    //-------------------------------------------------------------------------

    behavior of "tv"

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

    behavior of "inst"

    it should "throw an error when the number of provided types does not match the number of TGens" in {
        val a = TVar("a", Star)
        val sc = Forall(0, List(Star), TGen(0, 0) fn (TGen(0, 1) fn TGen(0, 0)))
        evaluating { inst(sc, List.empty, sc) } should produce [IndexOutOfBoundsException]
        evaluating { inst(sc, List(a), sc) } should produce [IndexOutOfBoundsException]
    }

    it should "replace TGens in the specified Forall with the provided types" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val sc = Forall(0, List(Star), TGen(0, 0) fn (a fn TGen(0, 0)))
        inst(sc, List(b), sc) should be === (b fn (a fn b))
    }

    it should "only replace TGens belonging to the specified Forall" in {
        val a = TVar("a", Star)
        val b = TVar("b", Star)
        val sc = Forall(0, List(Star), TGen(0, 0) fn Forall(1, List(Star), a fn TGen(1, 0)))
        inst(sc, List(b), sc) should be === (b fn Forall(1, List(Star), a fn TGen(1, 0)))
    }
}
