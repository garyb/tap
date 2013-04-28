package tap.interpreter

case class InterpreterError(msg: String) extends Exception(msg)
case class InterpreterRuntimeError(msg: String) extends RuntimeException(msg)
case class InterpreterMatchError(msg: String) extends Exception(msg)
