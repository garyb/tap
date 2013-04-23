package test.ast.parser

import test.ParserFixture
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.ast._
import tap.ast.parser.SExpressionParseException

class ParserTests extends FlatSpec with ParserFixture {

    // TODO: tests for things that shouldn't pass, more betterer tests generally

    behavior of "The s-expr parser"

    // ---[ values & expressions ]-------------------------------------------------------------------------------------

    it should "parse number values" in {
        parseExpr("10") should be === ASTNumber(10)
        parseExpr("0.1") should be === ASTNumber(0.1)
        parseExpr("0xABCDEF") should be === ASTNumber(0xABCDEF)
        parseExpr("0x12345") should be === ASTNumber(0x12345)
        parseExpr("0x67890") should be === ASTNumber(0x67890)
        // TODO: more thorough assertions
    }

    it should "parse string values" in {
        parseExpr(""""hello"""") should be === ASTString("hello")
        parseExpr(""""escape\" test"""") should be === ASTString("escape\" test")
        // TODO: more thorough assertions
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
        parseType("(forall (a) (-> a a))") should be === ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a"))))
        parseType("(-> (forall (a) (-> a a)) String)") should be === ASTFunctionType(List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))), ASTTypeCon("String")))
    }

    // ---[ modules ]--------------------------------------------------------------------------------------------------

    it should "parse module declarations" in {
        parseModule("(module Test)") should be ===
                ASTModule(
                    name = "Test",
                    exports = Nil,
                    imports = Nil,
                    datatypes = Nil,
                    typeclasses = Nil,
                    instances = Nil,
                    memberDefs = Nil,
                    memberImpls = Nil)
    }

    it should "parse imports" in {
        parseModule("""
            (module Test)
            (import Prelude)
        """) should be ===
                ASTModule(
                    name = "Test",
                    exports = Nil,
                    imports = List("Prelude"),
                    datatypes = Nil,
                    typeclasses = Nil,
                    instances = Nil,
                    memberDefs = Nil,
                    memberImpls = Nil)
    }

    it should "parse exports" in {
        // TODO: all the other kinds of export
        parseModule("""
            (module Test)
            (export (module Prelude))
        """) should be ===
                ASTModule(
                    name = "Test",
                    exports = List(ExModule("Prelude")),
                    imports = Nil,
                    datatypes = Nil,
                    typeclasses = Nil,
                    instances = Nil,
                    memberDefs = Nil,
                    memberImpls = Nil)
    }

    it should "parse member definitions" in {
        parseModule("""
            (module Test)
            (def identity (-> a a))
        """) should be ===
                ASTModule(
                    name = "Test",
                    exports = Nil,
                    imports = Nil,
                    datatypes = Nil,
                    typeclasses = Nil,
                    instances = Nil,
                    memberDefs = List(ASTDef("identity", Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a"))))),
                    memberImpls = Nil)
        parseModule("""
            (module Test)
            (def print (=> (Show a) (-> a String)))
        """) should be ===
                ASTModule(
                    name = "Test",
                    exports = Nil,
                    imports = Nil,
                    datatypes = Nil,
                    typeclasses = Nil,
                    instances = Nil,
                    memberDefs = List(ASTDef("print", List(ASTTypeClassReference("Show", List("a"))), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeCon("String"))))),
                    memberImpls = Nil)
    }

    it should "parse member implementations" in {
        parseModule("""
            (module Test)
            (let x 0)
        """) should be ===
                ASTModule(
                    name = "Test",
                    exports = List(ExMember("x")),
                    imports = Nil,
                    datatypes = Nil,
                    typeclasses = Nil,
                    instances = Nil,
                    memberDefs = Nil,
                    memberImpls = List(ASTLet("x", ASTNumber(0))))
    }

    it should "parse data type definitions" in {
        parseModule("""
            (module Test)
            (data Unit)
            (data Bool True False)
            (data (Maybe a) (Some a) None)
        """) should be ===
                ASTModule(
                    name = "Test",
                    exports = List(
                        ExDataType("Unit", Nil),
                        ExDataType("Bool", List("True", "False")),
                        ExDataType("Maybe", List("Some", "None"))),
                    imports = Nil,
                    datatypes = List(
                        ASTDataTypeDefinition("Unit", Nil, Nil),
                        ASTDataTypeDefinition("Bool", Nil, List(
                            ASTDataTypeConstructor("True", Nil),
                            ASTDataTypeConstructor("False", Nil))),
                        ASTDataTypeDefinition("Maybe", List("a"), List(
                            ASTDataTypeConstructor("Some", List(ASTTypeVar("a"))),
                            ASTDataTypeConstructor("None", Nil)))
                    ),
                    typeclasses = Nil,
                    instances = Nil,
                    memberDefs = Nil,
                    memberImpls = Nil)
    }

    ignore should "parse typeclass definitions" in {

    }

    ignore should "parse typeclass implementations" in {

    }
}
