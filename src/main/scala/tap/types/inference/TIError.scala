package tap.types.inference

import tap.ast.FilePositional
import tap.verifier.errors.{VerifierError, PositionedError}
import tap.types.{Meta, Type}
import tap.types.kinds.Kind._
import tap.util.PrettyPrint._

/**
 * Used for errors during type inference.
 */
case class TIError(msg: String, src: FilePositional) extends PositionedError(msg, src)

/**
 * Used for errors that should hopefully never occur and are definitely not caused by in the program being inferred.
 */
case class TIInternalError(msg: String) extends Error(msg)

case class TIUnifyError(x: Type, y: Type)
        extends VerifierError("Types do not unify:\n" +
                "    " + prettyPrint(x) + "\n" +
                "    " + prettyPrint(y))

case class TIUnifyOccursError(m: Meta, t: Type)
        extends VerifierError("Occurs check failed for " + prettyPrint(m) + " in " + prettyPrint(t))

case class TIUnifyKindError(x: Meta, y: Type)
        extends VerifierError("Types do not unify:\n" +
                "    " + prettyPrint(x) + " :: " + prettyPrint(kind(x)) + "\n" +
                "    " + prettyPrint(y) + " :: " + prettyPrint(kind(y)))