package tap.interpreter

import annotation.tailrec
import tap.ir._
import tap.{LocalId, ModuleId, Id}
import tap.types.Natives._
import scala.collection.mutable.ArrayBuffer

object Interpreter {

    sealed trait IValue
    case class IString(value: String) extends IValue
    case class INumber(value: Double) extends IValue
    case class INative(fn: IValue => IValue) extends IValue
    case class IFunctionExpr(fn: FunctionExpr, scope: Scope) extends IValue
    case class IData(dcon: Id, values: ArrayBuffer[IValue]) extends IValue

    type Scope = Map[Id, IValue]

    val iTrue: IValue = IData(idTrue, ArrayBuffer.empty)
    val iFalse: IValue = IData(idFalse, ArrayBuffer.empty)
    val iUnit: IValue = IData(idUnit, ArrayBuffer.empty)

    def eval(expr: TapExpr, scope: Scope): IValue = expr match {
        case BlockExpr(es) => es.foldLeft(iUnit) { (v, e) => eval(e, scope) }

        case ApplyExpr(func, value) =>
            eval(func, scope) match {
                case IFunctionExpr(FunctionExpr(arg, body), scope0) => eval(body, scope0 + (LocalId(arg.name) -> eval(value, scope)))
                case IData(id, values) => IData(id, values :+ eval(value, scope))
                case INative(fn) => fn(eval(value, scope))
                case f => throw InterpreterError("Non-function or dcon application: " + f)
            }

        case MatchExpr(expr, cases) => evalCases(cases, eval(expr, scope), scope)
        case LetExpr(name, value, inner) => eval(inner, scope + (LocalId(name) -> eval(value, scope)))
        case ValueReadExpr(id @ ModuleId("Native", _)) => InterpreterNatives.natives(id)
        case ValueReadExpr(ref) => scope(ref)
        case fn: FunctionExpr => IFunctionExpr(fn, scope)
        case CastExpr(value, _) => eval(value, scope)
        case RaiseErrorExpr(value) =>
            eval(value, scope) match {
                case IString(msg) => throw InterpreterRuntimeError("Interpreter runtime error: " + msg)
                case msg => throw InterpreterRuntimeError("Interpreter runtime error with invalid non-string error message: " + msg)
            }
        case StringExpr(s) => IString(s)
        case NumberExpr(n) => INumber(n)
    }

    @tailrec final def evalCases(xs: List[MatchCase], v: IValue, scope: Scope): IValue = xs match {
        case List() => throw InterpreterMatchError("Match error: " + v)
        case MatchCase(c, None, e) :: xs =>
            evalCase(c, v, scope) match {
                case Some((v, scope)) => eval(e, scope)
                case None => evalCases(xs, v, scope)
            }
        case MatchCase(c, Some(g), e) :: xs =>
            evalCase(c, v, scope) match {
                case Some((v, scope)) =>
                    val cond = eval(g, scope)
                    if (cond == iTrue) eval(e, scope)
                    else if (cond == iFalse) evalCases(xs, v, scope)
                    else throw InterpreterError("Non-boolean guard condition: " + g)
                case None => evalCases(xs, v, scope)
            }
    }

    @tailrec final def evalCase(c: PatternNode, v: IValue, scope: Scope): Option[(IValue, Scope)] = (c, v) match {
        case (UnapplyNode(dcon, args), v @ IData(dconVal, argVals)) if dcon == dconVal =>
            evalUnapplyArgs(args zip argVals, scope) match {
                case Some(scope) => Some((v, scope))
                case None => None
            }
        case (BindNode(name, Some(c)), v) => evalCase(c, v, scope + (LocalId(name) -> v))
        case (BindNode(name, None), v) => Some((v, scope + (LocalId(name) -> v)))
        case (StringExpr(s), v @ IString(sVal)) if s == sVal => Some(v, scope)
        case (NumberExpr(n), v @ INumber(nVal)) if n == nVal => Some(v, scope)
        case (WildcardValueExpr, v) => Some((v, scope))
        case _ => None
    }

    @tailrec final def evalUnapplyArgs(xs: List[(PatternNode, IValue)], scope: Scope): Option[Scope] = xs match {
        case List() => Some(scope)
        case (v, a) :: xs =>
            evalCase(v, a, scope) match {
                case Some((_, scope)) => evalUnapplyArgs(xs, scope)
                case None => None
            }
    }
}