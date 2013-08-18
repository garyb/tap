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

class ModuleTypeInferenceTests extends FlatSpec {

    behavior of "apply"
    ignore should "do some things" in {}

    // ------------------------------------------------------------------------

    behavior of "makeInstanceMemberType"

    ignore should "throw an error if passed a non-Forall type" in {}
    ignore should "throw an error if the instance type and forall'd variable counts differ" in {}

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
