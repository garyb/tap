package test.types.classes

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.types.kinds._
import tap.types.Type.{tString, tNumber, tBool, tList}
import tap.types.classes.{TypeclassDef, IsIn}
import tap.types.classes.IsIn._
import tap.types.classes.ClassEnvironments._
import tap.types._
import tap.ModuleId

class IsInTests extends FlatSpec with GivenWhenThen {

	behavior of "inst"

	it should "throw an error when the number of provided types does not match the number of TGens" in {
		val a = TVar(Tyvar("a", Star))
		val p = IsIn(ModuleId("Test", "TC"), List(TGen(0, 0), TGen(0, 1)))
		val sc = Forall(0, List(Star, Star), TAp(TGen(0, 0), TGen(0, 1)))
		evaluating { inst(sc, List.empty, p) } should produce [IndexOutOfBoundsException]
		evaluating { inst(sc, List(a), p) } should produce [IndexOutOfBoundsException]
	}

	it should "replace TGens in the specified IsIn with the provided types" in {
		val a = TVar(Tyvar("a", Star))
		val b = TVar(Tyvar("b", Star))
		val p = IsIn(ModuleId("Test", "TC"), List(TGen(0, 0), a))
		val sc = Forall(0, List(Star), TGen(0, 0) fn (a fn TGen(0, 0)))
		inst(sc, List(b), p) should be === IsIn(ModuleId("Test", "TC"), List(b, a))
	}

	it should "only replace TGens belonging to the specified Forall" in {
		val a = TVar(Tyvar("a", Star))
		val b = TVar(Tyvar("b", Star))
		val p = IsIn(ModuleId("Test", "TC"), List(TGen(0, 0), TGen(1, 0)))
		val sc = Forall(0, List(Star), TGen(0, 0) fn Forall(1, List(Star), a fn TGen(1, 0)))
		inst(sc, List(b), p) should be === IsIn(ModuleId("Test", "TC"), List(b, TGen(1, 0)))
	}

	//-------------------------------------------------------------------------

	behavior of "mguPred"

    it should "find the mgu for predicates with matching ids" in {
	    mguPred(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("a", Star)))), IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("b", Star))))) should be === Some(Map(Tyvar("a", Star) -> TVar(Tyvar("b", Star))))
    }

	it should "return nothing for mismatched predicates" in {
		mguPred(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("a", Star)))), IsIn(ModuleId("Test", "Y"), List(TVar(Tyvar("b", Star))))) should be === None
	}

	//-------------------------------------------------------------------------

	behavior of "matchPred"

	it should "find the match for predicates with matching ids" in {
		matchPred(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("a", Star)))), IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("b", Star))))) should be === Some(Map(Tyvar("a", Star) -> TVar(Tyvar("b", Star))))
	}

	it should "return nothing for mismatched predicates" in {
		matchPred(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("a", Star)))), IsIn(ModuleId("Test", "Y"), List(TVar(Tyvar("b", Star))))) should be === None
	}

	//-------------------------------------------------------------------------

	behavior of "bySuper"

	it should "return a list of predicates from the current typeclass if there are no superclasses" in {
		val ce = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ps = bySuper(ce, IsIn(ModuleId("Test", "X"), List(tString)))
		ps should be === List(IsIn(ModuleId("Test", "X"), List(tString)))
	}

	it should "return all the predicates from the current typeclass and all its superclasses" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test", "Y"), List(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("b", Star))))), List(Tyvar("b", Star)), Set.empty, Set.empty))
		val ce3 = addClass(ce2, TypeclassDef(ModuleId("Test", "Z"), List(IsIn(ModuleId("Test", "Y"), List(TVar(Tyvar("c", Star))))), List(Tyvar("c", Star)), Set.empty, Set.empty))
		val ps1 = bySuper(ce3, IsIn(ModuleId("Test", "Z"), List(tString)))
		ps1 should be === List(
			IsIn(ModuleId("Test", "Z"), List(tString)),
			IsIn(ModuleId("Test", "Y"), List(tString)),
			IsIn(ModuleId("Test", "X"), List(tString)))

		val ce4 = addClass(ce3, TypeclassDef(ModuleId("Test", "A"), List(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("p", Star)))), IsIn(ModuleId("Test", "Z"), List(TVar(Tyvar("q", Star))))), List(Tyvar("p", Star), Tyvar("q", Star)), Set.empty, Set.empty))
		val ps2 = bySuper(ce4, IsIn(ModuleId("Test", "A"), List(tNumber, tBool)))
		ps2 should be === List(
			IsIn(ModuleId("Test", "A"), List(tNumber, tBool)),
			IsIn(ModuleId("Test", "X"), List(tNumber)),
			IsIn(ModuleId("Test", "Z"), List(tBool)),
			IsIn(ModuleId("Test", "Y"), List(tBool)),
			IsIn(ModuleId("Test", "X"), List(tBool)))
	}

	//-------------------------------------------------------------------------

	behavior of "byInst"

	it should "return nothing if there is no matching inst" in {
		val ce = addClass(nullEnv, TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val p = IsIn(ModuleId("Test", "A"), List(tString))
		byInst(ce, p) should be === None
	}

	it should "return the predicates from the context of the matching inst" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test", "B"), Nil, List(Tyvar("b", Star)), Set.empty, Set.empty))

		when("a predicate that references an instance with no context")
		val ce3 = addInst(ce2, Inst("Test", Nil, IsIn(ModuleId("Test", "A"), List(tString))))
		val p1 = IsIn(ModuleId("Test", "A"), List(tString))

		then("the result should be an empty list, but not None")
		byInst(ce3, p1) should be === Some(List.empty)

		when("a predicate that references an instance with context")
		val ce4 = addInst(ce3, Inst("Test", List(IsIn(ModuleId("Test", "A"), List(TVar(Tyvar("x", Star))))), IsIn(ModuleId("Test", "B"), List(TAp(tList, TVar(Tyvar("x", Star)))))))
		val p2 = IsIn(ModuleId("Test", "B"), List(TAp(tList, tString)))

		then("the result should be list containing the require predicate(s)")
		byInst(ce4, p2) should be === Some(List(IsIn(ModuleId("Test", "A"), List(tString))))
	}

	//-------------------------------------------------------------------------

	behavior of "scEntail"

	it should "return true if p is present in ps" in {
		val ce = addClass(nullEnv, TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val p = IsIn(ModuleId("Test", "A"), List(tString))
		val ps = List(p)
		scEntail(ce, ps, p) should be (true)
	}

	it should "return true if p can be deduced from superclass information in ps" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test", "Y"), List(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("b", Star))))), List(Tyvar("b", Star)), Set.empty, Set.empty))
		val ps = List(IsIn(ModuleId("Test", "Y"), List(tString)))
		val p = IsIn(ModuleId("Test", "X"), List(tString))
		scEntail(ce2, ps, p) should be (true)
	}

	it should "return false if p cannot be deduced" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val p1 = IsIn(ModuleId("Test", "A"), List(tString))
		scEntail(ce1, Nil, p1) should be (false)

		val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test", "B"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ps = List(IsIn(ModuleId("Test", "B"), List(tString)))
		val p2 = IsIn(ModuleId("Test", "A"), List(tString))
		scEntail(ce2, ps, p2) should be (false)
	}

	//-------------------------------------------------------------------------

	behavior of "entail"

	it should "return true if p is present in ps" in {
		val ce = addClass(nullEnv, TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val p = IsIn(ModuleId("Test", "A"), List(tString))
		val ps = List(p)
		entail(ce, ps, p) should be (true)
	}

	it should "return true if p can be deduced from superclass information in ps" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test", "Y"), List(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("b", Star))))), List(Tyvar("b", Star)), Set.empty, Set.empty))
		val ps = List(IsIn(ModuleId("Test", "Y"), List(tString)))
		val p = IsIn(ModuleId("Test", "X"), List(tString))
		entail(ce2, ps, p) should be (true)
	}

	it should "return true if there is an instance for p and the instance predicates are satisfied" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "E"), Nil, List(Tyvar("a", Star)), Set(), Set()))
		val ce2 = addInst(ce1, Inst("Test", Nil, IsIn(ModuleId("Test", "E"), List(tString))))

		val p1 = IsIn(ModuleId("Test", "E"), List(tString))
		entail(ce2, Nil, p1) should be (true)

		val ce3 = addInst(ce2, Inst("Test", List(IsIn(ModuleId("Test", "E"), List(TVar(Tyvar("a", Star))))), IsIn(ModuleId("Test", "E"), List(TAp(tList, TVar(Tyvar("a", Star)))))))
		val p2 = IsIn(ModuleId("Test", "E"), List(TAp(tList, tString)))
		entail(ce3, Nil, p2) should be (true)
	}

	it should "return false if there is no instance for and no way of deducing p" in {
		val ce = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val p = IsIn(ModuleId("Test", "X"), List(tString))
		entail(ce, Nil, p) should be (false)
	}

	it should "return false if p's instance has predicates that are not satisfied by the types in p" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ce2 = addInst(ce1, Inst("Test", List(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("a", Star))))), IsIn(ModuleId("Test", "X"), List(TAp(tList, TVar(Tyvar("a", Star)))))))
		val p = IsIn(ModuleId("Test", "X"), List(TAp(tList, tString)))
		entail(ce2, Nil, p) should be (false)
	}

	//-------------------------------------------------------------------------

	behavior of "elimTauts"

	it should "remove predicates that always hold true in the current class environment" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test", "Y"), Nil, List(Tyvar("b", Star)), Set.empty, Set.empty))
		val ce3 = addInst(ce2, Inst("Test", Nil, IsIn(ModuleId("Test", "X"), List(tString))))
		val ce4 = addInst(ce3, Inst("Test", Nil, IsIn(ModuleId("Test", "Y"), List(tNumber))))
		val p1 = IsIn(ModuleId("Test", "X"), List(tString))
		val p2 = IsIn(ModuleId("Test", "X"), List(tNumber))
		val p3 = IsIn(ModuleId("Test", "Y"), List(tString))
		val p4 = IsIn(ModuleId("Test", "Y"), List(tNumber))
		elimTauts(ce4, List(p1, p2, p3, p4)) should be === List(p2, p3)
	}

	//-------------------------------------------------------------------------

	behavior of "reduce"

	it should "remove duplicate predicates" in {
		val ce = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val p = IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("b", Star))))
		reduce(ce, List(p, p)) should be === List(p)
	}

	it should "remove predicates that can be deduced from superclass information" in {
		val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test", "Y"), List(IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("b", Star))))), List(Tyvar("b", Star)), Set.empty, Set.empty))
		val p1 = IsIn(ModuleId("Test", "X"), List(tString))
		val p2 = IsIn(ModuleId("Test", "Y"), List(tString))
		reduce(ce2, List(p1, p2)) should be === List(p2)
	}

	//-------------------------------------------------------------------------

	"split" should "partition the predicates so the predicates containing specified type variables are on the left and the rest are on the right" in {
		val ce = addClass(nullEnv, TypeclassDef(ModuleId("Test", "X"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
		val p1 = IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("a", Star))))
		val p2 = IsIn(ModuleId("Test", "X"), List(TVar(Tyvar("b", Star))))
		split(ce, List(Tyvar("a", Star)), List(p1, p2)) should be === (List(p1), List(p2))
	}
 }
