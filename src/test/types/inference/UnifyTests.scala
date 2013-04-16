package test.types.inference

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.types._
import tap.types.Type._
import tap.types.inference.Unify._
import tap.types.inference.Substitutions._
import tap.types.kinds._
import tap.ast.NullFilePosition
import tap.types.inference.TIError

class UnifyTests extends FlatSpec {

	//-------------------------------------------------------------------------

	behavior of "varBind"

	it should "create nothing when the type variable and type have mismatched kinds" in {
		val a = Tyvar("a", Star)
		varBind(a, tList) should be === None
	}

	it should "create nothing when the variable is contained within the type being bound - a recursive type" in {
		val a = Tyvar("a", Star)
		varBind(a, TAp(tList, TVar(a))) should be === None
	}

	it should "create an empty substitution when a type variable and type are the same" in {
		val a = Tyvar("a", Star)
		varBind(a, TVar(a)) should be === Some(nullSubst)
	}

	it should "create a substitution for a type variable and type with matching kinds" in {
		val a = Tyvar("a", Star)
		varBind(a, tNumber) should be === Some(Map(a -> tNumber))
	}

	//-------------------------------------------------------------------------

	behavior of "mgu"

	it should "create a substitution for type variables" in {
		val a = Tyvar("a", Star)
		mgu(TVar(a), tString) should be === Some(Map(a -> tString))
		mgu(tString, TVar(a)) should be === Some(Map(a -> tString))
	}

	it should "create a substitution for type variables inside matching TAps" in {
		val a = Tyvar("a", Kfun(Star, Star))
		val b = Tyvar("b", Star)
		mgu(TAp(TVar(a), TVar(b)), TAp(tList, tString)) should
			be === Some(Map(a -> tList, b -> tString))
	}

	it should "create an empty substitution for matching type constructors" in {
		mgu(tString, tString) should be === Some(nullSubst)
	}

	it should "create nothing for mismatched type constructors" in {
		mgu(tString, tNumber) should be === None
	}

	it should "create an empty substitution for matching tgens" in {
		mgu(TGen(0, 0), TGen(0, 0)) should be === Some(nullSubst)
	}

	it should "create nothing for mismatched tgens" in {
		mgu(TGen(0, 0), TGen(1, 0)) should be === None
		mgu(TGen(0, 0), TGen(0, 1)) should be === None
	}

	it should "create a substitution for type variables inside matching foralls" in {
		val a = Tyvar("a", Star)
		mgu(Forall(0, List(Star), TAp(TGen(0, 0), TVar(a))), Forall(0, List(Star), TAp(TGen(0, 0), tString))) should be === Some(Map(a -> tString))
	}

	it should "create nothing for mismatched foralls" in {
		mgu(Forall(0, List(Star), TGen(0, 0)), Forall(1, List(Star), TGen(1, 0))) should be === None
		mgu(Forall(0, List(Star), TGen(0, 0)), Forall(0, List(Kfun(Star, Star)), TGen(0, 0))) should be === None
	}

	//-------------------------------------------------------------------------

	behavior of "mgus"

	it should "produce a nothing for mismatched lists of types" in {
		val a = Tyvar("a", Star)
		mgus(List(TVar(a), TVar(a)), List(tString, tNumber)) should be === None
		mgus(List(TVar(a)), List(tString, tNumber)) should be === None
		mgus(List(tNumber), List(tString, tString)) should be === None
	}

	it should "produce a substitution for lists of types" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Kfun(Star, Star))
		val c = Tyvar("c", Star)
		mgus(List(TVar(a), TAp(TVar(b), TVar(c))), List(tString, TAp(tList, tString))) should be === Some(Map(a -> tString, b -> tList, c -> tString))
	}

	//-------------------------------------------------------------------------

	behavior of "unify"

	it should "throw an error if there is no mgu for the specified types" in {
		evaluating { unify(tString, tNumber, nullSubst, NullFilePosition) } should produce [TIError]
	}

	it should "extend the current substitution with the mgu of two types" in {
		val a = Tyvar("a", Star)
		unify(TVar(a), tString, nullSubst, NullFilePosition) should be === Map(a -> tString)
	}

	it should "apply the current substitution before finding the mgu" in {
		val l = Tyvar("l", Kfun(Star, Star))
		val x = Tyvar("x", Star)
		val a = Tyvar("a", Star)
		val s = Map(x -> tString, l -> tList)
		unify(TAp(tList, TVar(a)), TAp(TVar(l), TVar(x)), s, NullFilePosition) should
			be === s + (a -> tString)
	}

	//-------------------------------------------------------------------------

	behavior of "`match`"

	it should "create a substitution only for type variables on the left" in {
		val a = Tyvar("a", Star)
		`match`(TVar(a), tString) should be === Some(Map(a -> tString))
		`match`(tString, TVar(a)) should be === None
	}

	it should "create a substitution for type variables inside matching TAps" in {
		val a = Tyvar("a", Kfun(Star, Star))
		val b = Tyvar("b", Star)
		`match`(TAp(TVar(a), TVar(b)), TAp(tList, tString)) should
			be === Some(Map(a -> tList, b -> tString))
	}

	it should "create an empty substitution for matching type constructors" in {
		`match`(tString, tString) should be === Some(nullSubst)
	}

	it should "create nothing for mismatched type constructors" in {
		`match`(tString, tNumber) should be === None
	}

	it should "create an empty substitution for matching tgens" in {
		`match`(TGen(0, 0), TGen(0, 0)) should be === Some(nullSubst)
	}

	it should "create nothing for mismatched tgens" in {
		`match`(TGen(0, 0), TGen(1, 0)) should be === None
		`match`(TGen(0, 0), TGen(0, 1)) should be === None
	}

	it should "create a substitution for type variables inside matching foralls" in {
		val a = Tyvar("a", Star)
		`match`(Forall(0, List(Star), TAp(TGen(0, 0), TVar(a))), Forall(0, List(Star), TAp(TGen(0, 0), tString))) should be === Some(Map(a -> tString))
	}

	it should "create nothing for mismatched foralls" in {
		`match`(Forall(0, List(Star), TGen(0, 0)), Forall(1, List(Star), TGen(1, 0))) should be === None
		`match`(Forall(0, List(Star), TGen(0, 0)), Forall(0, List(Kfun(Star, Star)), TGen(0, 0))) should be === None
	}

	//-------------------------------------------------------------------------

	behavior of "matches"

	it should "produce a nothing for mismatched lists of types" in {
		val a = Tyvar("a", Star)
		matches(List(TVar(a), TVar(a)), List(tString, tNumber)) should be === None
		matches(List(TVar(a)), List(tString, tNumber)) should be === None
		matches(List(tNumber), List(tString, tString)) should be === None
	}

	it should "produce a substitution for lists of types" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Kfun(Star, Star))
		val c = Tyvar("c", Star)
		matches(List(TVar(a), TAp(TVar(b), TVar(c))), List(tString, TAp(tList, tString))) should be === Some(Map(a -> tString, b -> tList, c -> tString))
	}
 }
