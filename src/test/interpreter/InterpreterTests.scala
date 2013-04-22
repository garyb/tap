package test.interpreter

import org.scalatest.matchers.ShouldMatchers._
import org.scalatest.{GivenWhenThen, FlatSpec}
import scala.Some
import tap.interpreter.Interpreter.{INumber, IString, iTrue, iFalse, iUnit}
import tap.interpreter.{InterpreterRuntimeError, InterpreterNatives}
import tap.ir._
import tap.types.Natives._
import tap.types.kinds.Star
import tap.types.{Natives, TCon, Tycon}
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

	ignore should "evaluate all expressions in a block" in {
		/*eval(VarExpr("xs", eEOL, BlockExpr(List(
			ValueSetExpr("xs", eCons(NumberExpr(3), ValueReadExpr(LocalId("xs")))),
			ValueSetExpr("xs", eCons(NumberExpr(2), ValueReadExpr(LocalId("xs")))),
			ValueSetExpr("xs", eCons(NumberExpr(1), ValueReadExpr(LocalId("xs")))),
			ValueReadExpr(LocalId("xs")))))) should be ===
		iCons(INumber(1), iCons(INumber(2), iCons(INumber(3), iEOL)))*/
	}

	it should "evaluate matches" in {

		// TODO: check only the correct branch is evaluated

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

		When("dealing with destructuring")
		eval(MatchExpr(eTuple2(eFalse, eTrue), List(
			MatchCase(UnapplyNode(idTuple2, List(WildcardValueExpr, BindNode("x", None))), None, ValueReadExpr(LocalId("x")))
		))) should be === iTrue

		When("dealing with complex value binding")
		eval(MatchExpr(eSome(eTrue), List(
			MatchCase(BindNode("x", Some(UnapplyNode(idSome, List(BindNode("v", None))))), None, eTuple2(ValueReadExpr(LocalId("x")), ValueReadExpr(LocalId("v"))))
		))) should be === iTuple2(iSome(iTrue), iTrue)
	}

	it should "evaluate the contents of cast expressions" in {
		eval(CastExpr(eTrue, TCon(Tycon(ModuleId("Prelude", "Boolean"), Star)))) should be === iTrue
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

	ignore should "evaluate native set!" in {
		//eval(ApplyExpr(ApplyExpr(native(`Num+Num`), NumberExpr(1)), NumberExpr(2))) should be === INumber(1 + 2)
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
