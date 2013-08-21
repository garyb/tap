package tap.interpreter

import tap.ModuleId
import tap.interpreter.Interpreter._
import tap.types.Natives._
import scala.collection.mutable.ArrayBuffer

object InterpreterNatives {

    def fn(fn: IValue => IValue): INative = INative(fn)
    def fn2(fn: List[IValue] => IValue): INative = INative(x => INative(y => fn(List(x, y))))

    val natives: Map[ModuleId, IValue] = Map(

        idUnit -> iUnit,
        idTrue -> iTrue,
        idFalse -> iFalse,

        idVar -> fn {
            x => IData(idVar, ArrayBuffer(x))
        },

        `get!` -> fn {
            case IData(id, items) if id == idVar => items(0)
            case x => throw InterpreterError("Invalid type")
        },

        `set!` -> fn2 {
            case List(v @ IData(id, items), x) if id == idVar =>
                items(0) = x
                v
            case _ => throw InterpreterError("Invalid type")
        },

        `Num+Num` -> fn2 {
            case List(INumber(x), INumber(y)) => INumber(x + y)
            case _ => throw InterpreterError("Invalid type")
        },

        `String+String` -> fn2 {
            case List(IString(x), IString(y)) => IString(x + y)
            case _ => throw InterpreterError("Invalid type")
        },

        `Num-Num` -> fn2 {
            case List(INumber(x), INumber(y)) => INumber(x - y)
            case _ => throw InterpreterError("Invalid type")
        },

        `Num/Num` -> fn2 {
            case List(INumber(x), INumber(y)) => INumber(x / y)
            case _ => throw InterpreterError("Invalid type")
        },

        `Num*Num` -> fn2 {
            case List(INumber(x), INumber(y)) => INumber(x * y)
            case _ => throw InterpreterError("Invalid type")
        },

        `Num%Num` -> fn2 {
            case List(INumber(x), INumber(y)) => INumber(x % y)
            case _ => throw InterpreterError("Invalid type")
        },

        `-Num` -> fn {
            case INumber(x) => INumber(-x)
            case _ => throw InterpreterError("Invalid type")
        },

        `write-to-console` -> fn {
            case IString(x) =>
                System.out.println("> " + x)
                iUnit
            case _ => throw InterpreterError("Invalid type")
        },

        `Num==Num` -> fn2 {
            case List(INumber(x), INumber(y)) => if (x == y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `String==String` -> fn2 {
            case List(IString(x), IString(y)) => if (x == y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `Num>Num` -> fn2 {
            case List(INumber(x), INumber(y)) => if (x > y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `Num<Num` -> fn2 {
            case List(INumber(x), INumber(y)) => if (x < y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `String>String` -> fn2 {
            case List(IString(x), IString(y)) => if (x > y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `String<String` -> fn2 {
            case List(IString(x), IString(y)) => if (x < y) iTrue else iFalse
            case _ => throw InterpreterError("Invalid type")
        },

        `showNum` -> fn {
            case INumber(x) => IString(x.toString)
            case _ => throw InterpreterError("Invalid type")
        },

        `readNum` -> fn {
            case IString(n) => INumber(java.lang.Double.parseDouble(n))
            case _ => throw InterpreterError("Invalid type")
        }
    )

}
