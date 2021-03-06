package test.ast

import org.scalatest.matchers.ShouldMatchers._
import org.scalatest.{GivenWhenThen, FlatSpec}
import tap.ModuleId
import tap.ast.ASTUtil._
import tap.ast._
import tap.types.Type._
import tap.types.Natives._
import tap.types._
import tap.types.kinds._
import tap.verifier.errors._
import language.reflectiveCalls
import tap.verifier.defs.ModuleDefinitions
import tap.types.inference.TIEnv

class ASTUtilTests extends FlatSpec with GivenWhenThen {

    val nullEnv = TIEnv.empty

    // ————————————————————————————————————————————————————————————————————————

    behavior of "getTConName"

    it should "extract the name from an ASTTypeCon and return the correct ID" in {
        val lookup = Map(
            "TestA" -> ModuleId("Test", "TestA"),
            "TestB" -> ModuleId("Test", "TestB")
        )
        getTConName(lookup, ASTTypeCon("TestA")) should be === ModuleId("Test", "TestA")
    }

    it should "extract the name from an applied ASTTypeCon" in {
        val lookup = Map("Test" -> ModuleId("Test", "Test"))
        getTConName(lookup, ASTTypeApply(ASTTypeCon("Test"), List(ASTTypeVar("a")))) should be === ModuleId("Test", "Test")
    }

    it should "throw an error if the type contains no type constructor" in {
        evaluating { getTConName(Map.empty, ASTTypeVar("a")) } should produce [Error]
        evaluating { getTConName(Map.empty, ASTTypeApply(ASTTypeVar("a"), Nil)) } should produce [Error]
        evaluating { getTConName(Map.empty, ASTFunctionType(Nil)) } should produce [Error]
        evaluating { getTConName(Map.empty, ASTForall(Nil, ASTTypeVar("a"))) } should produce [Error]
    }

    it should "throw an error if the type constructor is missing from the lookup" in {
        evaluating { getTConName(Map.empty, ASTTypeCon("Test")) } should produce [NoSuchElementException]
    }

    // ————————————————————————————————————————————————————————————————————————

    behavior of "findTypeConstructors"

    it should "extract the name from an ASTTypeCon" in {
        val lookup = Map("Test" -> ModuleId("Test", "Test"))
        findTypeConstructors(lookup, ASTTypeCon("Test")) should be ===
                Set(ModuleId("Test", "Test"))
    }

    it should "throw an error if a type constructor is missing from the lookup" in {
        evaluating { findTypeConstructors(Map.empty, ASTTypeCon("Test")) } should produce [UnknownTypeConstructorError]
    }

    it should "extract nothing from an ASTTypeVars" in {
        findTypeConstructors(Map.empty, ASTTypeVar("a")) should be === Set.empty
    }

    it should "extract the names inside an ASTTypeApply" in {
        val lookup = Map(
            "TestA" -> ModuleId("Test", "TestA"),
            "TestB" -> ModuleId("Test", "TestB"),
            "TestC" -> ModuleId("Test", "TestC")
        )
        findTypeConstructors(lookup, ASTTypeApply(ASTTypeCon("TestA"), List(ASTTypeCon("TestB"), ASTTypeCon("TestC")))) should be ===
                Set(ModuleId("Test", "TestA"), ModuleId("Test", "TestB"), ModuleId("Test", "TestC"))
    }

    it should "extract the names inside an ASTFunctionType" in {
        val lookup = Map(
            "TestA" -> ModuleId("Test", "TestA"),
            "TestB" -> ModuleId("Test", "TestB")
        )
        findTypeConstructors(lookup, ASTFunctionType(List(ASTTypeCon("TestA"), ASTTypeCon("TestB")))) should be ===
                Set(ModuleId("Test", "TestA"), ModuleId("Test", "TestB"))
    }

    it should "extract the names inside an ASTForall" in {
        val lookup = Map("Test" -> ModuleId("Test", "Test"))
        findTypeConstructors(lookup, ASTForall(List("a"), ASTTypeCon("Test"))) should be ===
                Set(ModuleId("Test", "Test"))
    }

    // ————————————————————————————————————————————————————————————————————————

    behavior of "findAllTypeConstructors"

    it should "extract the names from a list of ASTTypes" in {
        val lookup = Map(
            "TestA" -> ModuleId("Test", "TestA"),
            "TestB" -> ModuleId("Test", "TestB"),
            "TestC" -> ModuleId("Test", "TestC"),
            "TestD" -> ModuleId("Test", "TestD"),
            "TestE" -> ModuleId("Test", "TestE"),
            "TestF" -> ModuleId("Test", "TestF"),
            "TestG" -> ModuleId("Test", "TestG")
        )
        val types = List(
            ASTTypeCon("TestA"),
            ASTTypeVar("a"),
            ASTTypeApply(ASTTypeCon("TestB"), List(ASTTypeCon("TestC"), ASTTypeCon("TestD"))),
            ASTFunctionType(List(ASTTypeCon("TestE"), ASTTypeCon("TestF"))),
            ASTForall(List("a"), ASTTypeCon("TestG"))
        )
        findAllTypeConstructors(lookup, types) should be ===
                Set(ModuleId("Test", "TestA"),
                    ModuleId("Test", "TestB"),
                    ModuleId("Test", "TestC"),
                    ModuleId("Test", "TestD"),
                    ModuleId("Test", "TestE"),
                    ModuleId("Test", "TestF"),
                    ModuleId("Test", "TestG"))
    }

    // ————————————————————————————————————————————————————————————————————————

    behavior of "findTypeVars"

    it should "extract the names from a ASTTypeVar" in {
        findTypeVars(ASTTypeVar("a")) should be === Set("a")
    }

    it should "extract nothing from an ASTTypeCon" in {
        findTypeVars(ASTTypeCon("Test")) should be === Set.empty
    }

    it should "extract the names from an ASTTypeApply" in {
        findTypeVars(ASTTypeApply(ASTTypeVar("a"), List(ASTTypeVar("b"), ASTTypeVar("c")))) should be ===
                Set("a", "b", "c")
    }

    it should "extract the names from an ASTFunctionType" in {
        findTypeVars(ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("b"), ASTTypeVar("c")))) should be ===
                Set("a", "b", "c")
    }

    it should "throw an error if type variables appearing in a forall overlap with the current result" in {
        evaluating {
            findTypeVars(ASTForall(List("a"), ASTTypeVar("a")), Set("a"))
        } should produce [TypeVariableOverlapError]
    }

    it should "extract the names from an ASTForall" in {
        findTypeVars(ASTForall(List("a"), ASTTypeVar("b"))) should be ===
                Set("a", "b")
    }

    // ————————————————————————————————————————————————————————————————————————

    behavior of "getType"

    it should "return the correct TCon" in {
        val lookup = Map(
            "TestA" -> ModuleId("Test", "TestA"),
            "TestB" -> ModuleId("Test", "TestB")
        )
        val tcons = Map(
            ModuleId("Test", "TestA") -> TCon(ModuleId("Test", "TestA"), Star)
        )
        getType(nullEnv, lookup, tcons, Map.empty, ASTTypeCon("TestA"))._3 should be ===
                TCon(ModuleId("Test", "TestA"), Star)
    }

    it should "throw an error if a TCon is missing from the lookup" in {
        evaluating { getType(nullEnv, Map.empty, Map.empty, Map.empty, ASTTypeCon("Test")) } should produce [NoSuchElementException]
    }

    it should "throw an error if a TCon is missing from the tcons" in {
        val lookup = Map("Test" -> ModuleId("Test", "Test"))
        evaluating { getType(nullEnv, lookup, Map.empty, Map.empty, ASTTypeCon("Test")) } should produce [UnknownTypeConstructorError]
    }

    it should "return the correct TVar in" in {
        val tvs = Map(
            "a" -> TVar("a", Star),
            "b" -> TVar("b", Star)
        )
        getType(nullEnv, Map.empty, Map.empty, tvs, ASTTypeVar("b"))._3 should be ===
            tvs("b")
    }

    it should "throw an error if a TVar is missing from the tvs" in {
        evaluating { getType(nullEnv, Map.empty, Map.empty, Map.empty, ASTTypeVar("a")) } should produce [UnknownTypeVariableError]
    }

    it should "return TAps" in {
        val tvs = Map(
            "a" -> TVar("a", Kfun(Star, Kfun(Star, Star))),
            "b" -> TVar("b", Star)
        )
        getType(nullEnv, Map.empty, Map.empty, tvs, ASTTypeApply(ASTTypeVar("a"), List(ASTTypeVar("b"), ASTTypeVar("b"))))._3 should be ===
                TAp(TAp(tvs("a"), tvs("b")), tvs("b"))
    }

    it should "return function types without arguments" in {
        val tvs = Map("a" -> TVar("a", Star))

        getType(nullEnv, Map.empty, Map.empty, tvs, ASTFunctionType(List(ASTTypeVar("a"))))._3 should be ===
                TAp(TAp(tArrow, tUnit), tvs("a"))
    }

    it should "return function types with arguments" in {
        val tvs = Map(
            "a" -> TVar("a", Star),
            "b" -> TVar("b", Star),
            "c" -> TVar("c", Star)
        )

        getType(nullEnv, Map.empty, Map.empty, tvs, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("b"))))._3 should be ===
                TAp(TAp(tArrow, tvs("a")), tvs("b"))

        getType(nullEnv, Map.empty, Map.empty, tvs, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("b"), ASTTypeVar("c"))))._3 should be ===
                TAp(TAp(tArrow, tvs("a")), TAp(TAp(tArrow, tvs("b")), tvs("c")))
    }

    it should "return Foralls as needed" in {
        val lookup = Map("String" -> ModuleId("Native", "String"))
        val tcons = ModuleDefinitions.defaults.tcons
        val t1 = getType(nullEnv, lookup, tcons, Map.empty, ASTFunctionType(List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))), ASTTypeCon("String"))))._3
        t1 should be === (Forall(0, List(Star), TGen(0, 0) fn TGen(0, 0)) fn tString)

        When("quantified variables already exist in the tvs list, they should be hidden inside the forall")
        val tvs = Map("a" -> TVar("a", Star), "b" -> TVar("b", Star))
        val t2 = getType(nullEnv, lookup, tcons, tvs, ASTFunctionType(List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("b"), ASTTypeVar("a")))), ASTTypeVar("a"))))._3
        t2 should be === (Forall(0, List(Star), TGen(0, 0) fn (tvs("b") fn TGen(0, 0))) fn tvs("a"))
    }

    it should "throw an error if a type is applied too many parameters" in {
        val lookup = Map(
            "List" -> ModuleId("Prelude", "List"),
            "String" -> ModuleId("Prelude", "String")
        )
        val tcons = Map(
            ModuleId("Prelude", "List") -> TCon(ModuleId("Prelude", "List"), Kfun(Star, Star)),
            ModuleId("Prelude", "String") -> TCon(ModuleId("Prelude", "String"), Star)
        )

        evaluating {
            getType(nullEnv, lookup, tcons, Map.empty, ASTTypeApply(ASTTypeCon("List"), List(ASTTypeCon("String"), ASTTypeCon("String"))))
        } should produce [TypeConstructorTooManyArgsError]

        evaluating {
            getType(nullEnv, lookup, tcons, Map.empty, ASTTypeApply(ASTTypeCon("String"), List(ASTTypeCon("String"))))
        } should produce [TypeConstructorNoArgsError]
    }

    it should "return substitutions for type variables replaced with TGens in a Forall" in {
        val s = getType(nullEnv, Map.empty, Map.empty, Map.empty, ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))))._2
        s should be === Map(TVar("a", Star) -> TGen(0, 0))
    }

    it should "return substitutions for type variables replaced with TGens in a Forall inside a ASTTypeApply" in {
        val lookup = Map("Maybe" -> ModuleId("Prelude", "Maybe"))
        val tcons = Map(ModuleId("Prelude", "Maybe") -> TCon(ModuleId("Prelude", "Maybe"), Kfun(Star, Star)))
        val s = getType(nullEnv, lookup, tcons, Map.empty, ASTTypeApply(ASTTypeCon("Maybe"), List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))))))._2
        s should be === Map(TVar("a", Star) -> TGen(0, 0))
    }

    it should "return substitutions for type variables replaced with TGens in a Forall inside a ASTFunctionType" in {
        val lookup = Map("String" -> ModuleId("Prelude", "String"))
        val tcons = Map(ModuleId("Prelude", "String") -> TCon(ModuleId("Prelude", "String"), Star))
        val s = getType(nullEnv, lookup, tcons, Map.empty, ASTFunctionType(List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))), ASTTypeCon("String"))))._2
        s should be === Map(TVar("a", Star) -> TGen(0, 0))
    }
}
