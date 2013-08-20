package test

import tap.interpreter.Interpreter
import tap.interpreter.Interpreter._
import tap.ir._
import tap.{LocalId, ModuleId}
import scala.collection.mutable.ArrayBuffer
import tap.types.Natives._

trait InterpreterFixture {

    val idTuple2 = ModuleId("Prelude.Tuple", "Tuple2")
    val idCons = ModuleId("Prelude", ":")
    val idEOL = ModuleId("Prelude", "EOL")
    val idSome = ModuleId("Prelude", "Some")
    val idNone = ModuleId("Prelude", "None")

    val defaultScope: Scope = Map(
        idTrue -> iTrue,
        idFalse -> iFalse,
        idUnit -> iUnit,
        idVar -> IData(idVar, ArrayBuffer.empty),
        idTuple2 -> IData(idTuple2, ArrayBuffer.empty),
        idCons -> IData(idCons, ArrayBuffer.empty),
        idEOL -> IData(idEOL, ArrayBuffer.empty),
        idSome -> IData(idSome, ArrayBuffer.empty),
        idNone -> IData(idNone, ArrayBuffer.empty)
    )

    def eval(expr: TapExpr, scope: Scope = Map.empty): IValue = {
        Interpreter.eval(expr, defaultScope ++ scope)
    }

    val eTrue = ValueReadExpr(idTrue)
    val eFalse = ValueReadExpr(idFalse)
    val eUnit = ValueReadExpr(idUnit)

    def eSome(e: TapExpr) = ApplyExpr(ValueReadExpr(idSome), e)
    def iSome(e: IValue) = IData(idSome, ArrayBuffer(e))

    val eNone = ValueReadExpr(idNone)
    val iNone = IData(idNone, ArrayBuffer.empty)

    def eTuple2(x: TapExpr, y: TapExpr) = ApplyExpr(ApplyExpr(ValueReadExpr(idTuple2), x), y)
    def iTuple2(x: IValue, y: IValue) = IData(idTuple2, ArrayBuffer(x, y))

    def eCons(x: TapExpr, y: TapExpr) = ApplyExpr(ApplyExpr(ValueReadExpr(idCons), x), y)
    def iCons(x: IValue, y: IValue) = IData(idCons, ArrayBuffer(x, y))

    val iEOL = IData(idEOL, ArrayBuffer.empty)
    val eEOL = ValueReadExpr(idEOL)

    def eVar(x: TapExpr) = ApplyExpr(ValueReadExpr(idVar), x)
    def iVar(x: IValue) = IData(idVar, ArrayBuffer(x))

    val eFnIdentity = FunctionExpr(Argument("x"), ValueReadExpr(LocalId("x")))
}
