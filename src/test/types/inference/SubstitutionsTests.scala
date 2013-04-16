package test.types.inference

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.types._
import tap.types.Type._
import tap.types.kinds._
import tap.types.inference.Substitutions._
import tap.ModuleId
import tap.types.classes.{Qual, IsIn}
import scala.Some

class SubstitutionsTests extends FlatSpec {

	behavior of "applySubst for Type"

	it should "do nothing when encountering a type var that is not in the substitutions" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		val x = Tyvar("x", Star)
		applySubst(Map(a -> TVar(x)), TVar(b)) should be === TVar(b)
	}

	it should "return the new type when encountering a type var is in the substitutions" in {
		val a = Tyvar("a", Star)
		val x = Tyvar("x", Star)
		applySubst(Map(a -> TVar(x)), TVar(a)) should be === TVar(x)
	}

	it should "apply substitutions to type vars in a TAp" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		val x = Tyvar("x", Star)
		val y = Tyvar("y", Star)
		applySubst(Map(a -> TVar(x), b -> TVar(y)), TAp(TVar(a), TVar(b))) should be === TAp(TVar(x), TVar(y))
	}

	it should "apply substitutions to type vars in a Forall" in {
		val a = Tyvar("a", Star)
		val x = Tyvar("x", Star)
		applySubst(Map(a -> TVar(x)), Forall(0, Nil, TVar(a))) should be === Forall(0, Nil, TVar(x))
	}

	it should "do nothing when encountering other types" in {
		val a = Tyvar("a", Star)
		val x = Tyvar("x", Star)
		val s = Map(a -> TVar(x))
		applySubst(s, TCon(Tycon(ModuleId("Prelude", "Number"), Star))) should be === TCon(Tycon(ModuleId("Prelude", "Number"), Star))
		applySubst(s, TGen(0, 0)) should be === TGen(0, 0)
	}

	//-------------------------------------------------------------------------

	"applySubst for IsIn" should "apply substitutions to the predicates list" in {
		val a = Tyvar("a", Star)
		val x = Tyvar("x", Star)
		val s = Map(a -> TVar(x))
		applySubst(s, IsIn(ModuleId("Test", "Test"), List(TVar(a)))) should be === IsIn(ModuleId("Test", "Test"), List(TVar(x)))
	}

	//-------------------------------------------------------------------------

	"applySubst for Qual[Type]" should "apply substitutions to the predicates list and type" in {
		val a = Tyvar("a", Star)
		val x = Tyvar("x", Star)
		val s = Map(a -> TVar(x))
		applySubst(s, Qual(List(IsIn(ModuleId("Test", "Test"), List(TVar(a)))), TVar(a))) should be === Qual(List(IsIn(ModuleId("Test", "Test"), List(TVar(x)))), TVar(x))
	}

	//-------------------------------------------------------------------------

	behavior of "tv for Type"

	it should "return a type variable" in {
		val a = Tyvar("a", Star)
		tv(TVar(a)) should be === List(a)
	}

	it should "return all the type variables in a type application" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		tv(TAp(TVar(a), TVar(b))) should be === List(a, b)
	}

	it should "return all the unquantified type variables in a forall" in {
		val a = Tyvar("a", Star)
		tv(Forall(0, Nil, TVar(a))) should be === List(a)
		tv(Forall(0, List(Star), TAp(TVar(a), TGen(0, 0)))) should be === List(a)
	}

	it should "return nothing for other types" in {
		tv(TCon(Tycon(ModuleId("Prelude", "String"), Star))) should be === Nil
		tv(TGen(0, 0)) should be === Nil
	}

	it should "return a list of distinct type variables" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		tv(Forall(0, List(Star), TAp(TVar(a), TVar(b) fn TVar(a) fn TGen(0, 0) fn TVar(b)))) should be === List(a, b)
	}

	//-------------------------------------------------------------------------

	"tv for IsIn" should "return a list of distinct type variables from the types list" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		tv(IsIn(ModuleId("Test", "Test"), List(TVar(a), TAp(TVar(a), TVar(b))))) should be === List(a, b)
	}

	//-------------------------------------------------------------------------

	"tv for Qual[Type]" should "return a list of distinct type variables from the predicates list and type" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		val c = Tyvar("c", Star)
		tv(Qual(List(IsIn(ModuleId("Test", "Test"), List(TVar(a), TAp(TVar(a), TVar(b))))), TVar(c) fn TVar(c))) should be === List(a, b, c)
	}

	//-------------------------------------------------------------------------

	"@@" should "compose substitutions" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		val c = Tyvar("c", Star)
		val x = Tyvar("x", Star)
		val y = Tyvar("y", Star)
		val q = Tyvar("q", Star)
		val s1 = Map(x -> tString, y -> TVar(q))
		val s2 = Map(a -> TVar(x), b -> TAp(TVar(y), TVar(c)))
		val s3 = s1 @@ s2
		s3 should be === Map(a -> tString, b -> TAp(TVar(q), TVar(c)), x -> tString, y -> TVar(q))
	}

	//-------------------------------------------------------------------------

	behavior of "merge"

	it should "return None if the substitutions have conflicting entrie" in {
		val a = Tyvar("a", Star)
		val x = Tyvar("x", Star)
		val y = Tyvar("y", Star)
		val s1 = Map(a -> TVar(x))
		val s2 = Map(a -> TVar(y))
		merge(s1, s2) should be === None
	}

	it should "return the combined substitution if the substitutions have no overlap" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		val x = Tyvar("x", Star)
		val y = Tyvar("y", Star)
		val s1 = Map(a -> TVar(x))
		val s2 = Map(b -> TVar(y))
		merge(s1, s2) should be === Some(Map(a -> TVar(x), b -> TVar(y)))
	}

	it should "return the combined substitution if the substitutions equivalent entries" in {
		val a = Tyvar("a", Star)
		val b = Tyvar("b", Star)
		val c = Tyvar("c", Star)
		val x = Tyvar("x", Star)
		val y = Tyvar("y", Star)
		val z = Tyvar("z", Star)
		val s1 = Map(a -> TVar(x), c -> TVar(z))
		val s2 = Map(b -> TVar(y), c -> TVar(z))
		merge(s1, s2) should be === Some(Map(a -> TVar(x), b -> TVar(y), c -> TVar(z)))
	}
 }
