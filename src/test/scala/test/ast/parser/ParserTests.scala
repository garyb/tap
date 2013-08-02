package test.ast.parser

import test.ParserFixture
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.ast._
import tap.ast.parser.SExpressionParseException

class ParserTests extends FlatSpec with ParserFixture {

    behavior of "The s-expr parser"

    // ---[ values & expressions ]-------------------------------------------------------------------------------------

    it should "parse number values" in {
        parseExpr("10") should be === ASTNumber(10)
        parseExpr("0.1") should be === ASTNumber(0.1)
        parseExpr("0xABCDEF") should be === ASTNumber(0xABCDEF)
        parseExpr("0x12345") should be === ASTNumber(0x12345)
        parseExpr("0x67890") should be === ASTNumber(0x67890)
    }

    it should "parse string values" in {
        parseExpr(""""hello"""") should be === ASTString("hello")
        parseExpr(""""escape\" test"""") should be === ASTString("escape\" test")
    }

    it should "parse native and wildcard special values" in {
        parseExpr("native") should be === ASTNativeValue
        parseExpr("_") should be === ASTWildcardValue
    }

    it should "parse references" in {
        parseExpr("a") should be === ASTValueRead("a")
        parseExpr("Something") should be === ASTValueRead("Something")
    }

    it should "parse let special forms" in {
        parseExpr("(let x 0)") should be === ASTLet("x", ASTNumber(0))
    }

    it should "parse begin special form" in {
        parseExpr("(begin (let x 0) x)") should be === ASTBlock(List(ASTLet("x", ASTNumber(0)), ASTValueRead("x")))
    }

    it should "parse lambda special form" in {
        parseExpr("(lambda () 0)") should be === ASTFunction(Nil, ASTNumber(0))
        parseExpr("(lambda (x) x)") should be === ASTFunction(List("x"), ASTValueRead("x"))
        parseExpr("(lambda (x y) y)") should be === ASTFunction(List("x", "y"), ASTValueRead("y"))
        parseExpr("(lambda (x y) x y)") should be === ASTFunction(List("x", "y"), ASTBlock(List(ASTValueRead("x"), ASTValueRead("y"))))
    }

    it should "parse special tuple syntax" in {
        parseExpr("'(x)") should be === ASTApply(ASTValueRead("Tuple1"), List(ASTValueRead("x")))
        parseExpr("'(x y)") should be === ASTApply(ASTValueRead("Tuple2"), List(ASTValueRead("x"), ASTValueRead("y")))
        parseExpr("'(x y z)") should be === ASTApply(ASTValueRead("Tuple3"), List(ASTValueRead("x"), ASTValueRead("y"), ASTValueRead("z")))
        intercept[SExpressionParseException](parseExpr("'()"))
    }

    it should "parse special array syntax" in {
        parseExpr("[x]") should be === ASTApply(ASTValueRead(":"), List(ASTValueRead("x"), ASTValueRead("EOL")))
        parseExpr("[x y]") should be === ASTApply(ASTValueRead(":"), List(ASTValueRead("x"), ASTApply(ASTValueRead(":"), List(ASTValueRead("y"), ASTValueRead("EOL")))))
        parseExpr("[]") should be === ASTValueRead("EOL")
    }

    it should "parse and special forms" in {
        parseExpr("(and x y)") should be ===
            ASTMatch(ASTValueRead("x"), List(
                ASTCaseBranch(ASTValueRead("True"), None,
                    ASTMatch(ASTValueRead("y"), List(
                        ASTCaseBranch(ASTValueRead("True"), None, ASTValueRead("True")),
                        ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("False"))))),
                ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("False"))))

        parseExpr("(and x y z)") should be ===
            ASTMatch(ASTValueRead("x"), List(
                ASTCaseBranch(ASTValueRead("True"), None, ASTMatch(ASTValueRead("y"), List(
                        ASTCaseBranch(ASTValueRead("True"), None, ASTMatch(ASTValueRead("z"), List(
                                ASTCaseBranch(ASTValueRead("True"), None, ASTValueRead("True")),
                                ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("False"))))),
                        ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("False"))))),
                ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("False"))))
    }

    it should "parse or special forms" in {
        parseExpr("(or x y)") should be ===
            ASTMatch(ASTValueRead("x"), List(
                ASTCaseBranch(ASTValueRead("False"), None,
                    ASTMatch(ASTValueRead("y"), List(
                        ASTCaseBranch(ASTValueRead("False"), None, ASTValueRead("False")),
                        ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("True"))))),
                ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("True"))))

        parseExpr("(or x y z)") should be ===
            ASTMatch(ASTValueRead("x"), List(
                ASTCaseBranch(ASTValueRead("False"), None, ASTMatch(ASTValueRead("y"), List(
                        ASTCaseBranch(ASTValueRead("False"), None, ASTMatch(ASTValueRead("z"), List(
                                ASTCaseBranch(ASTValueRead("False"), None, ASTValueRead("False")),
                                ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("True"))))),
                        ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("True"))))),
                ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("True"))))
    }

    it should "parse if special form" in {
        parseExpr("(if x 0 1)") should be ===
            ASTMatch(ASTValueRead("Unit"), List(
                ASTCaseBranch(ASTWildcardValue, Some(ASTValueRead("x")), ASTNumber(0)),
                ASTCaseBranch(ASTWildcardValue, None, ASTNumber(1))))

        parseExpr("(if x 1 y 2 0)") should be ===
            ASTMatch(ASTValueRead("Unit"), List(
                ASTCaseBranch(ASTWildcardValue, Some(ASTValueRead("x")), ASTNumber(1)),
                ASTCaseBranch(ASTWildcardValue, Some(ASTValueRead("y")), ASTNumber(2)),
                ASTCaseBranch(ASTWildcardValue, None, ASTNumber(0))))
    }

    it should "parse match special form" in {
        parseExpr("""
            (match x
                (case 0 True)
                (case "foo" True)
                (case x x)
                (case (Some x) x)
                (case _ True)
                (case '(_ x) x)
                (case [x (Some y)] x)
                (case-if x (test x) x))
            """) should be ===
                ASTMatch(ASTValueRead("x"), List(
                    ASTCaseBranch(ASTNumber(0), None, ASTValueRead("True")),
                    ASTCaseBranch(ASTString("foo"), None, ASTValueRead("True")),
                    ASTCaseBranch(ASTValueRead("x"), None, ASTValueRead("x")),
                    ASTCaseBranch(ASTUnapply("Some", List(ASTValueRead("x"))), None, ASTValueRead("x")),
                    ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("True")),
                    ASTCaseBranch(ASTUnapply("Tuple2", List(ASTWildcardValue, ASTValueRead("x"))), None, ASTValueRead("x")),
                    ASTCaseBranch(ASTBind("x", ASTUnapply("Some", List(ASTValueRead("y")))), None, ASTValueRead("x")),
                    ASTCaseBranch(ASTValueRead("x"), Some(ASTApply(ASTValueRead("test"), List(ASTValueRead("x")))), ASTValueRead("x"))
                ))
    }

    it should "parse error special form" in {
        parseExpr("(error x)") should be === ASTRaiseError(ASTValueRead("x"))
    }

    it should "parse cast special form" in {
        parseExpr("(cast x Number)") should be === ASTCast(ASTValueRead("x"), ASTTypeCon("Number"))
    }

    it should "parse data constructor application" in {
        parseExpr("(Some 5)") should be === ASTApply(ASTValueRead("Some"), List(ASTNumber(5)))
    }

    it should "parse function application" in {
        parseExpr("(identity 5)") should be === ASTApply(ASTValueRead("identity"), List(ASTNumber(5)))
    }

    // ---[ types ]-------------------------------------------------------------------------------------

    it should "parse type constructors" in {
        parseType("Number") should be === ASTTypeCon("Number")
    }

    it should "parse type variables" in {
        parseType("a") should be === ASTTypeVar("a")
    }

    it should "parse type applications" in {
        parseType("(Maybe a)") should be === ASTTypeApply(ASTTypeCon("Maybe"), List(ASTTypeVar("a")))
        parseType("(Either a b)") should be === ASTTypeApply(ASTTypeCon("Either"), List(ASTTypeVar("a"), ASTTypeVar("b")))
        parseType("(m a)") should be === ASTTypeApply(ASTTypeVar("m"), List(ASTTypeVar("a")))
    }

    it should "parse function types" in {
        parseType("(-> a b c)") should be === ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("b"), ASTTypeVar("c")))
        parseType("(-> String String)") should be === ASTFunctionType(List(ASTTypeCon("String"), ASTTypeCon("String")))
        parseType("(-> String)") should be === ASTFunctionType(List(ASTTypeCon("String")))
    }

    it should "parse tuple type syntax" in {
        parseType("'(Number a)") should be === ASTTypeApply(ASTTypeCon("Tuple2"), List(ASTTypeCon("Number"), ASTTypeVar("a")))
    }

    it should "parse array type syntax" in {
        parseType("[]") should be === ASTTypeCon("List")
        parseType("[a]") should be === ASTTypeApply(ASTTypeCon("List"), List(ASTTypeVar("a")))
    }

    it should "parse universally quantified types" in {
        parseType("(forall (a) a)") should be === ASTForall(List("a"), ASTTypeVar("a"))
        parseType("(forall (a) (-> a a))") should be === ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a"))))
        parseType("(-> (forall (a) (-> a a)) String)") should be === ASTFunctionType(List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))), ASTTypeCon("String")))
    }

    // ---[ modules ]--------------------------------------------------------------------------------------------------

    it should "parse module declarations" in {
        parseModule("(module Test)") should be ===
                ASTModule("Test", Nil)
    }

    it should "parse imports" in {
        parseModule("""
            (module Test)
            (import Prelude)
        """) should be ===
                ASTModule("Test", List(
                    ASTImport("Prelude", None, None)))
    }

    it should "parse data type exports" in {
        parseModule("""
            (module Test)
            (export (data TCon1 (DCon1 DCon2)))
            (export (data TCon2 ()))
        """) should be ===
                ASTModule("Test", List(
                    ASTDataTypeExport("TCon1", Set("DCon1", "DCon2")),
                    ASTDataTypeExport("TCon2", Set())))
    }

    it should "parse typeclass exports" in {
        parseModule("""
            (module Test)
            (export (class SomeClass))
        """) should be ===
                ASTModule("Test", List(
                    ASTClassExport("SomeClass")))
    }

    it should "parse member exports" in {
        parseModule("""
            (module Test)
            (export x)
        """) should be ===
                ASTModule("Test", List(
                    ASTMemberExport("x")))
    }

    it should "parse module exports" in {
        parseModule("""
            (module Test)
            (export (module Prelude))
        """) should be ===
                ASTModule("Test", List(
                    ASTModuleExport("Prelude")))
    }

    it should "parse member definitions" in {
        parseModule("""
            (module Test)
            (def identity (-> a a))
        """) should be ===
                ASTModule("Test", List(
                    ASTDef("identity", ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))))))
        parseModule("""
            (module Test)
            (def print (=> (Show a) (-> a String)))
        """) should be ===
                ASTModule("Test", List(
                    ASTDef("print", ASTQType(List(ASTClassRef("Show", List("a"))), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeCon("String")))))))
    }

    it should "parse member implementations" in {
        parseModule("""
            (module Test)
            (let x 0)
        """) should be ===
                ASTModule("Test", List(
                    ASTLet("x", ASTNumber(0))))
    }

    it should "parse data type definitions" in {
        parseModule("""
            (module Test)
            (data Unit)
            (data Bool True False)
            (data (Maybe a) (Some a) None)
        """) should be ===
                ASTModule("Test", List(
                        ASTDataType("Unit", Nil, Nil),
                        ASTDataType("Bool", Nil, List(
                            ASTDataCon("True", Nil),
                            ASTDataCon("False", Nil))),
                        ASTDataType("Maybe", List("a"), List(
                            ASTDataCon("Some", List(ASTTypeVar("a"))),
                            ASTDataCon("None", Nil)))))
    }

    it should "parse typeclass definitions" in {
        parseModule("""
            (module Test)
            (class Ord (=> (Eq a)) (a)
              (def nonsense (=> (Ord b) (-> a b Bool)))
              (def compare (-> a a Ordering))
              (let compare (lambda (x y) EQ)))
        """) should be ===
                ASTModule("Test", List(
                    ASTClass("Ord", List(ASTClassRef("Eq", List("a"))), List("a"), List(
                        ASTClassMemberDef("nonsense", ASTQType(List(ASTClassRef("Ord", List("b"))), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("b"), ASTTypeCon("Bool"))))),
                        ASTClassMemberDef("compare", ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a"), ASTTypeCon("Ordering"))))),
                        ASTClassMemberImpl("compare", ASTFunction(List("x", "y"), ASTValueRead("EQ")))
                    ))))
    }

    it should "parse typeclass instances" in {
        parseModule("""
            (module Test)
            (instance Eq (Unit)
              (let == (lambda (x y) True)))
            (instance Eq (=> (Eq a)) ((List a))
              (let == (lambda (lx ly) False)))
        """) should be ===
                ASTModule("Test", List(
                    ASTClassInst("Eq", Nil, List(ASTTypeCon("Unit")), List(
                        ASTClassMemberImpl("==", ASTFunction(List("x", "y"), ASTValueRead("True")))
                    )),
                    ASTClassInst("Eq", List(ASTClassRef("Eq", List("a"))), List(ASTTypeApply(ASTTypeCon("List"), List(ASTTypeVar("a")))), List(
                        ASTClassMemberImpl("==", ASTFunction(List("lx", "ly"), ASTValueRead("False")))
                    ))))
    }
}
