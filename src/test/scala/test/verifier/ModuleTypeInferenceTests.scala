package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.types.classes._
import tap.ModuleId
import tap.types.kinds._
import tap.types._
import tap.types.Type._
import language.reflectiveCalls
import tap.types.classes.ClassEnvironments._
import tap.verifier.ModuleTypeInference

class ModuleTypeInferenceTests extends FlatSpec with GivenWhenThen {

    behavior of "apply"
    ignore should "do some things" in {}

    // ------------------------------------------------------------------------

    behavior of "makeInstanceMemberType"

    it should "throw an error if passed a non-Forall type" in {
        val sc = Qual(List(IsIn(ModuleId("Prelude", "Show"), List(TVar("a", Star)))), TVar("a", Star) fn tString)
        val tci = Inst("Test", Nil, IsIn(ModuleId("Prelude", "Show"), List(tNumber)))
        val mti = new ModuleTypeInference(Nil, Map.empty, Map.empty)
        evaluating {
            mti.makeInstanceMemberType(sc, tci)
        } should produce [Error]
    }

    it should "throw an error if the instance type and forall'd variable counts differ" in {
        Given("too many types on the instance")
        evaluating {
            val sc = Qual(List(IsIn(ModuleId("Test", "MultiClass"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn tString))
            val tci = Inst("Test", Nil, IsIn(ModuleId("Test", "MultiClass"), List(tNumber, tString)))
            val mti = new ModuleTypeInference(Nil, Map.empty, Map.empty)
            mti.makeInstanceMemberType(sc, tci)
        } should produce [Error]

        Given("not enough types on the instance")
        evaluating {
            val sc = Qual(List(IsIn(ModuleId("Test", "MultiClass"), List(TGen(0, 0), TGen(0, 1)))), Forall(0, List(Star, Star), (TGen(0, 0) fn TGen(0, 1)) fn tString))
            val tci = Inst("Test", Nil, IsIn(ModuleId("Test", "MultiClass"), List(tNumber)))
            val mti = new ModuleTypeInference(Nil, Map.empty, Map.empty)
            mti.makeInstanceMemberType(sc, tci)
        } should produce [Error]
    }

    it should "instantiate a type for a particular class instance" in {
        val sc = Qual(List(IsIn(ModuleId("Prelude", "Show"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn tString))
        val tci = Inst("Test", Nil, IsIn(ModuleId("Prelude", "Show"), List(tNumber)))
        val mti = new ModuleTypeInference(Nil, Map.empty, Map.empty)
        mti.makeInstanceMemberType(sc, tci) should be ===
                Qual(List(IsIn(ModuleId("Prelude", "Show"), List(tNumber))), tNumber fn tString)
    }

    ignore should "instantiate a type for a particular class instance when the class has multiple parameters" in {}

    // ------------------------------------------------------------------------

    behavior of "buildClassEnv"
    ignore should "do some things" in {}

    // ------------------------------------------------------------------------

    behavior of "resolveBindingGroups"
    ignore should "do some things" in {}

}
