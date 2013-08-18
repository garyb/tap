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

    it should "instantiate a type for a particular class instance" in {
        val sc = Qual(List(IsIn(ModuleId("Prelude", "Show"), List(TVar("a", Star)))), TVar("a", Star) fn tString)
        val tci = Inst("Test", Nil, IsIn(ModuleId("Prelude", "Show"), List(tNumber)))
        val mti = new ModuleTypeInference(Nil, Map.empty, Map.empty)
        mti.makeInstanceMemberType(sc, tci) should be ===
                Qual(Nil, tNumber fn tString)
    }

    ignore should "instantiate a type for a particular class instance when the class has multiple parameters" in {}

    // ------------------------------------------------------------------------

    behavior of "buildClassEnv"
    ignore should "do some things" in {}

    // ------------------------------------------------------------------------

    behavior of "resolveBindingGroups"
    ignore should "do some things" in {}

}
