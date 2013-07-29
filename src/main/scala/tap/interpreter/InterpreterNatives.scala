package tap.interpreter

import tap.{ModuleId, Id}
import tap.interpreter.Interpreter._
import tap.types.Natives._

object InterpreterNatives {

    def make2(fn: List[IValue] => IValue): IValue => IValue = {
        x => INative(y => fn(List(x, y)))
    }

    val natives: Map[Id, IValue => IValue] = Map(

        `get!` -> {
            case IData(ModuleId("Prelude", "Var"), items) => items(0)
            case _ => throw InterpreterError("Invalid type")
        },

        `set!` -> make2 {
            case List(IData(ModuleId("Prelude", "Var"), items), x) =>
                items(0) = x
                x
            case _ => throw InterpreterError("Invalid type")
        },

        `Num+Num` -> make2 {
            case List(INumber(x), INumber(y)) => INumber(x + y)
            case _ => throw InterpreterError("Invalid type")
        },

        `String+String` -> make2 {
            case List(IString(x), IString(y)) => IString(x + y)
            case _ => throw InterpreterError("Invalid type")
        },

        `Num-Num` -> make2 {
            case List(INumber(x), INumber(y)) => INumber(x - y)
            case _ => throw InterpreterError("Invalid type")
        },

        `Num/Num` -> make2 {
            case List(INumber(x), INumber(y)) => INumber(x / y)
            case _ => throw InterpreterError("Invalid type")
        },

        `Num*Num` -> make2 {
            case List(INumber(x), INumber(y)) => INumber(x * y)
            case _ => throw InterpreterError("Invalid type")
        },

        `Num%Num` -> make2 {
            case List(INumber(x), INumber(y)) => INumber(x % y)
            case _ => throw InterpreterError("Invalid type")
        },

        `-Num` -> {
            case INumber(x) => INumber(-x)
            case _ => throw InterpreterError("Invalid type")
        },

        `write-to-console` -> {
            case IString(x) =>
                System.out.println("> " + x)
                iUnit
            case _ => throw InterpreterError("Invalid type")
        },

        `Num==Num` -> make2 {
            case List(INumber(x), INumber(y)) => if (x == y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `String==String` -> make2 {
            case List(IString(x), IString(y)) => if (x == y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `Num>Num` -> make2 {
            case List(INumber(x), INumber(y)) => if (x > y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `Num<Num` -> make2 {
            case List(INumber(x), INumber(y)) => if (x < y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `String>String` -> make2 {
            case List(IString(x), IString(y)) => if (x > y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `String<String` -> make2 {
            case List(IString(x), IString(y)) => if (x < y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `showNum` -> {
            case INumber(x) => IString(x.toString)
            case _ => throw InterpreterError("Invalid type")
        },

        `readNum` -> {
            case IString(n) => INumber(java.lang.Double.parseDouble(n))
            case _ => throw InterpreterError("Invalid type")
        }
    )

}