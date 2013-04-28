package tap.types.inference

import tap.ast.FilePositional
import tap.verifier.errors.PositionedError

/**
 * Used for errors during type inference.
 */
case class TIError(msg: String, src: FilePositional) extends PositionedError(msg, src)

/**
 * Used for errors that should hopefully never occur and are definirely not caused by in the program being inferred.
 */
case class TIInternalError(msg: String) extends Error(msg)