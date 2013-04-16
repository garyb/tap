package tap.verifier.errors

import tap.Id
import tap.ast.FilePositional

// ---[ assignments ]----------------------------------------------------------

case class ConstantAssignmentError(ident: String, src: FilePositional)
		extends PositionedError("Cannot assign to constant '" + ident + "'", src)

case class IllegalAssignmentError(ident: Id, src: FilePositional)
		extends PositionedError("Module members are constant and cannot be assigned to: attempted to assign to '" + ident + "'", src)

// ---[ matches & patterns ]---------------------------------------------------

case class NoCaseError(src: FilePositional)
		extends PositionedError("Match has no cases", src)

case class DuplicatePatternBind(ident: String, src: FilePositional)
		extends PositionedError("Pattern has duplicate binding '" + ident + "'", src)

// ---[ underscores ]----------------------------------------------------------

case class IllegalUnderscoreError(src: FilePositional)
		extends PositionedError("Illegal usage of _", src)

// ---[ native usages ]--------------------------------------------------------

case class IllegalNativeError(src: FilePositional)
		extends PositionedError("Illegal usage of `native`", src)

case class InvalidNativeError(id: Id, src: FilePositional)
		extends PositionedError("There is no native implementation for " + id, src)

// ---[ recursion ]------------------------------------------------------------

case class IllegalRecursionError(ident: String, src: FilePositional)
		extends PositionedError("Variable definition " + ident + " contains a recursive reference. Recursive references are only allowed in constant definitions.", src)