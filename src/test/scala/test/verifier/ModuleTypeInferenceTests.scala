package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.types.classes._
import tap._
import tap.types.kinds._
import tap.types._
import tap.types.Type._
import tap.types.Natives._
import language.reflectiveCalls
import tap.types.classes.ClassEnvironments._
import tap.verifier.ModuleTypeInference
import tap.ir._

class ModuleTypeInferenceTests extends FlatSpec with GivenWhenThen {

    behavior of "apply"
    ignore should "do some things" in {}

    // ------------------------------------------------------------------------

    behavior of "makeInstanceMemberType"

    it should "throw an error if instantiating a type that does not have predicates for the instance class" in {
        val sc = Qual(List(IsIn(ModuleId("Prelude", "Show"), List(TGen(0, 0)))), Forall(0, List(Star), TGen(0, 0) fn tString))
        val tci = Inst("Test", Nil, IsIn(ModuleId("Test", "NotShow"), List(tNumber)))
        val mti = new ModuleTypeInference(Nil, Map.empty, Map.empty)
        evaluating {
            mti.makeInstanceMemberType(sc, tci)
        } should produce [Error]
    }

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

    it should "instantiate a type for a particular class instance when the class has multiple parameters" in {
        val sc = Qual(List(IsIn(ModuleId("Test", "MultiClass"), List(TGen(0, 0), TGen(0, 1)))), Forall(0, List(Star, Star), (TGen(0, 0) fn TGen(0, 1)) fn tString))
        val tci = Inst("Test", Nil, IsIn(ModuleId("Test", "MultiClass"), List(tNumber, tString)))
        val mti = new ModuleTypeInference(Nil, Map.empty, Map.empty)
        mti.makeInstanceMemberType(sc, tci) should be ===
                Qual(List(IsIn(ModuleId("Test", "MultiClass"), List(tNumber, tString))), (tNumber fn tString) fn tString)
    }

    // ------------------------------------------------------------------------

    behavior of "buildClassEnv"
    ignore should "do some things" in {}

    // ------------------------------------------------------------------------

    behavior of "resolveBindingGroups"

    it should "group implicit definitions as tightly as possible" in {

        /*
            Equivalent to:

            (def tmpX (-> a a))
            (let tmpX (lambda (x) (tmpD x) x))

            (let tmpA (lambda (a) (tmpB a) a))
            (let tmpB (lambda (b) (tmpA b) (tmpX b) b))

            (let tmpC (lambda (c) (tmpA c) (tmpD c) c))
            (let tmpD (lambda (d) (tmpC d) d))

            Or as a .dot graph:

            digraph {
              X [shape=box]
              A -> B
              B -> A
              B -> X
              C -> A
              C -> D
              D -> C
              X -> D
            }
        */

        val mis: Map[Id, TapExpr] = Map(
            ModuleId("Prelude","tmpC") -> FunctionExpr(Argument("c"),BlockExpr(List(ApplyExpr(ValueReadExpr(ModuleId("Prelude","tmpA")),ValueReadExpr(LocalId("c"))), ApplyExpr(ValueReadExpr(ModuleId("Prelude","tmpD")),ValueReadExpr(LocalId("c"))), ValueReadExpr(LocalId("c"))))),
            ModuleId("Prelude","tmpX") -> FunctionExpr(Argument("x"),BlockExpr(List(ApplyExpr(ValueReadExpr(ModuleId("Prelude","tmpD")),ValueReadExpr(LocalId("x"))), ValueReadExpr(LocalId("x"))))),
            ModuleId("Prelude","tmpB") -> FunctionExpr(Argument("b"),BlockExpr(List(ApplyExpr(ValueReadExpr(ModuleId("Prelude","tmpA")),ValueReadExpr(LocalId("b"))), ApplyExpr(ValueReadExpr(ModuleId("Prelude","tmpX")),ValueReadExpr(LocalId("b"))), ValueReadExpr(LocalId("b"))))),
            ModuleId("Prelude","tmpD") -> FunctionExpr(Argument("d"),BlockExpr(List(ApplyExpr(ValueReadExpr(ModuleId("Prelude","tmpC")),ValueReadExpr(LocalId("d"))), ValueReadExpr(LocalId("d"))))),
            ModuleId("Prelude","tmpA") -> FunctionExpr(Argument("a"),BlockExpr(List(ApplyExpr(ValueReadExpr(ModuleId("Prelude","tmpB")),ValueReadExpr(LocalId("a"))), ValueReadExpr(LocalId("a")))))
        )

        val mts: Map[Id, Qual[Type]] = Map(
            ModuleId("Prelude","tmpX") -> Qual(List(),Forall(6,List(Star),TAp(TAp(TCon(ModuleId("Prelude","->"),Kfun(Star,Kfun(Star,Star))),TGen(6,0)),TGen(6,0))))
        )

        val mti = new ModuleTypeInference(Nil, Map.empty, Map.empty)
        val bgs = mti.resolveBindingGroups(mis, mts)

        val expl = List(ModuleId("Prelude","tmpX"))
        val impls = List(
            List(ModuleId("Prelude","tmpB"), ModuleId("Prelude","tmpA")),
            List(ModuleId("Prelude","tmpD"), ModuleId("Prelude","tmpC"))
        )

        bgs should be === List((expl, impls))
    }

    ignore should "do some other things" in {}

}
