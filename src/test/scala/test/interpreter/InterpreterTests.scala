package test.interpreter

import org.scalatest.matchers.ShouldMatchers._
import org.scalatest.{GivenWhenThen, FlatSpec}
import scala.Some
import tap.interpreter.Interpreter.{INumber, IString, iTrue, iFalse, iUnit}
import tap.interpreter.{InterpreterRuntimeError, InterpreterNatives}
import tap.ir._
import tap.types.Natives._
import tap.types.kinds.Star
import tap.types.{Natives, TCon}
import tap.{Id, LocalId, ModuleId}
import test.InterpreterFixture

class InterpreterTests extends FlatSpec with GivenWhenThen with InterpreterFixture {

    behavior of "The interpreter"

    it should "return number inputs" in {
        eval(NumberExpr(0)) should be === INumber(0)
    }

    it should "return string inputs" in {
        eval(StringExpr("foo")) should be === IString("foo")
    }

    it should "evaluate references" in {
        eval(eUnit) should be === iUnit
    }

    it should "evaluate declarations" in {
        eval(LetExpr("foo", eTrue, ValueReadExpr(LocalId("foo")))) should be === iTrue
    }

    it should "evaluate data constructor application" in {
        eval(eTuple2(eTrue, eFalse)) should be === iTuple2(iTrue, iFalse)
    }

    it should "evaluate function application" in {
        eval(ApplyExpr(eFnIdentity, eTrue)) should be === iTrue
    }

    it should "evaluate all expressions in a block" in {
        eval(LetExpr("xs", eVar(eEOL), BlockExpr(List(
            ApplyExpr(ApplyExpr(native(`set!`), ValueReadExpr(LocalId("xs"))), eCons(NumberExpr(3), ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs"))))),
            ApplyExpr(ApplyExpr(native(`set!`), ValueReadExpr(LocalId("xs"))), eCons(NumberExpr(2), ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs"))))),
            ApplyExpr(ApplyExpr(native(`set!`), ValueReadExpr(LocalId("xs"))), eCons(NumberExpr(1), ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs"))))),
            ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs"))))))) should be ===
        iCons(INumber(1), iCons(INumber(2), iCons(INumber(3), iEOL)))
    }

    it should "evaluate matches" in {

        When("dealing with wildcard matching")
        eval(MatchExpr(eTrue, List(
            MatchCase(WildcardValueExpr, None, eTrue)
        ))) should be === iTrue

        When("dealing with string value matching")
        eval(MatchExpr(StringExpr("hello"), List(
            MatchCase(StringExpr("hello"), None, eTrue)
        ))) should be === iTrue

        When("dealing with number value matching")
        eval(MatchExpr(NumberExpr(20), List(
            MatchCase(NumberExpr(20), None, eTrue)
        ))) should be === iTrue

        When("dealing with basic value binding")
        eval(MatchExpr(eTrue, List(
            MatchCase(BindNode("x", None), None, ValueReadExpr(LocalId("x")))
        ))) should be === iTrue

        When("dealing with basic data constructor matching")
        eval(MatchExpr(eFalse, List(
            MatchCase(UnapplyNode(idTrue, Nil), None, NumberExpr(1)),
            MatchCase(UnapplyNode(idFalse, Nil), None, NumberExpr(2))
        ))) should be === INumber(2)

        When("dealing with destructuring")
        eval(MatchExpr(eTuple2(eFalse, eTrue), List(
            MatchCase(UnapplyNode(idTuple2, List(WildcardValueExpr, BindNode("x", None))), None, ValueReadExpr(LocalId("x")))
        ))) should be === iTrue

        When("dealing with complex value binding")
        eval(MatchExpr(eSome(eTrue), List(
            MatchCase(BindNode("x", Some(UnapplyNode(idSome, List(BindNode("v", None))))), None, eTuple2(ValueReadExpr(LocalId("x")), ValueReadExpr(LocalId("v"))))
        ))) should be === iTuple2(iSome(iTrue), iTrue)
    }

    it should "evaluate matches with guards" in {

        eval(MatchExpr(eTrue, List(
            MatchCase(UnapplyNode(idTrue, Nil), Some(ApplyExpr(ApplyExpr(native(`Num==Num`), NumberExpr(1)), NumberExpr(1))), NumberExpr(1)),
            MatchCase(UnapplyNode(idTrue, Nil), None, NumberExpr(2))
        ))) should be === INumber(1)

        eval(MatchExpr(eTrue, List(
            MatchCase(UnapplyNode(idTrue, Nil), Some(ApplyExpr(ApplyExpr(native(`Num==Num`), NumberExpr(1)), NumberExpr(2))), NumberExpr(1)),
            MatchCase(UnapplyNode(idTrue, Nil), Some(ApplyExpr(ApplyExpr(native(`Num==Num`), NumberExpr(2)), NumberExpr(2))), NumberExpr(2))
        ))) should be === INumber(2)
    }

    it should "only evaluate the correct branch when matching" in {

        eval(LetExpr("xs", eVar(eEOL), BlockExpr(List(
            MatchExpr(eFalse, List(
                MatchCase(UnapplyNode(idTrue, Nil), None, ApplyExpr(ApplyExpr(native(`set!`), ValueReadExpr(LocalId("xs"))), eCons(NumberExpr(0), ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs")))))),
                MatchCase(UnapplyNode(idFalse, Nil), None, ApplyExpr(ApplyExpr(native(`set!`), ValueReadExpr(LocalId("xs"))), eCons(NumberExpr(1), ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs"))))))
            )),
            ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs"))))))) should be ===
                iCons(INumber(1), iEOL)

        eval(LetExpr("xs", eVar(eEOL), BlockExpr(List(
            MatchExpr(eTrue, List(
                MatchCase(UnapplyNode(idTrue, Nil), Some(ApplyExpr(ApplyExpr(native(`Num==Num`), NumberExpr(1)), NumberExpr(1))), ApplyExpr(ApplyExpr(native(`set!`), ValueReadExpr(LocalId("xs"))), eCons(NumberExpr(0), ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs")))))),
                MatchCase(UnapplyNode(idTrue, Nil), None, ApplyExpr(ApplyExpr(native(`set!`), ValueReadExpr(LocalId("xs"))), eCons(NumberExpr(1), ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs"))))))
            )),
            ApplyExpr(native(`get!`), ValueReadExpr(LocalId("xs"))))))) should be ===
                iCons(INumber(0), iEOL)
    }

    it should "evaluate the contents of cast expressions" in {
        eval(CastExpr(eTrue, TCon(ModuleId("Prelude", "Boolean"), Star))) should be === iTrue
    }

    it should "throw an exception When a runtime error is raised" in {
        evaluating { eval(RaiseErrorExpr(StringExpr("test"))) } should produce [InterpreterRuntimeError]
    }

    it should "have a mapping for each native value" in {
        val nativeIds = Natives.types.keySet
        val implIds = InterpreterNatives.natives.keySet
        nativeIds should be === implIds
    }

    def native(id: Id): NativeValueExpr = NativeValueExpr(id, Natives.types(id))

    it should "evaluate native get!" in {
        eval(ApplyExpr(native(`get!`), ApplyExpr(ValueReadExpr(idVar), NumberExpr(1)))) should be === INumber(1)
    }

    it should "evaluate native set!" in {
        eval(ApplyExpr(ApplyExpr(native(`set!`), eVar(NumberExpr(1))), NumberExpr(2))) should be === iVar(INumber(2))
    }

    it should "evaluate native num/num +" in {
        eval(ApplyExpr(ApplyExpr(native(`Num+Num`), NumberExpr(1)), NumberExpr(2))) should be === INumber(1 + 2)
    }

    it should "evaluate native str/str +" in {
        eval(ApplyExpr(ApplyExpr(native(`String+String`), StringExpr("foo")), StringExpr("bar"))) should be === IString("foo" + "bar")
    }

    it should "evaluate native num/num -" in {
        eval(ApplyExpr(ApplyExpr(native(`Num-Num`), NumberExpr(1)), NumberExpr(2))) should be === INumber(1 - 2)
    }

    it should "evaluate native num/num /" in {
        eval(ApplyExpr(ApplyExpr(native(`Num/Num`), NumberExpr(1)), NumberExpr(2))) should be === INumber(1.0 / 2.0)
    }

    it should "evaluate native num/num *" in {
        eval(ApplyExpr(ApplyExpr(native(`Num*Num`), NumberExpr(1)), NumberExpr(2))) should be === INumber(1 * 2)
    }

    it should "evaluate native num/num mod" in {
        eval(ApplyExpr(ApplyExpr(native(`Num%Num`), NumberExpr(1)), NumberExpr(2))) should be === INumber(1 % 2)
    }

    it should "evaluate native num/num negate" in {
        eval(ApplyExpr(native(`-Num`), NumberExpr(1))) should be === INumber(-1)
    }

    it should "evaluate native write-to-console" in {
        eval(ApplyExpr(native(`write-to-console`), StringExpr("hello world"))) should be === iUnit
    }

    it should "evaluate native num/num ==" in {
        eval(ApplyExpr(ApplyExpr(native(`Num==Num`), NumberExpr(1)), NumberExpr(2))) should be === iFalse
        eval(ApplyExpr(ApplyExpr(native(`Num==Num`), NumberExpr(3)), NumberExpr(3))) should be === iTrue
    }

    it should "evaluate native str/str ==" in {
        eval(ApplyExpr(ApplyExpr(native(`String==String`), StringExpr("foo")), StringExpr("bar"))) should be === iFalse
        eval(ApplyExpr(ApplyExpr(native(`String==String`), StringExpr("abc")), StringExpr("abc"))) should be === iTrue
    }

    it should "evaluate native num/num <" in {
        eval(ApplyExpr(ApplyExpr(native(`Num<Num`), NumberExpr(1)), NumberExpr(2))) should be === iTrue
    }

    it should "evaluate native num/num >" in {
        eval(ApplyExpr(ApplyExpr(native(`Num>Num`), NumberExpr(1)), NumberExpr(2))) should be === iFalse
    }

    it should "evaluate native str/str <" in {
        eval(ApplyExpr(ApplyExpr(native(`String<String`), StringExpr("a")), StringExpr("b"))) should be === iTrue
    }

    it should "evaluate native str/str >" in {
        eval(ApplyExpr(ApplyExpr(native(`String>String`), StringExpr("a")), StringExpr("b"))) should be === iFalse
    }

    it should "evaluate native num show" in {
        eval(ApplyExpr(native(`showNum`), NumberExpr(1))) should be === IString(1.0.toString)
    }

    it should "evaluate native num read" in {
        eval(ApplyExpr(native(`readNum`), StringExpr("1"))) should be === INumber(java.lang.Double.parseDouble("1"))
    }
}
