package tap.verifier.errors

import tap.ast.FilePositional
import tap.types.TCon
import tap.types.kinds.Kind
import tap.util.PrettyPrint._

case class UnknownTypeConstructorError(id: String, src: FilePositional)
		extends PositionedError("Unknown type constructor '" + id + "'", src)

case class UnknownTypeVariableError(tvName: String, src: FilePositional)
		extends PositionedError("Type variable '" + tvName + "' is not in scope", src)

case class UnknownTypeclassError(tcName: String, src: FilePositional)
		extends PositionedError("Unknown typeclass '" + tcName + "'", src)

case class KindMismatchError(id: String, required: Kind, actual: Kind, src: FilePositional)
		extends PositionedError("Type '" + id + "' should have kind: " + prettyPrint(required) + ", actual: " + prettyPrint(actual), src)

case class KindConflictError(x: Kind, y: Kind, src: FilePositional)
		extends PositionedError("Conflicting kinds: '" + prettyPrint(x) + "' and '" + prettyPrint(y) + "'", src)

// ---[ type constructor arity ]-----------------------------------------------

case class TypeConstructorNoArgsError(tcon: TCon, src: FilePositional)
		extends PositionedError("Type constructor '" + tcon.c.id + "' does not accept type arguments.", src)

case class TypeConstructorTooManyArgsError(tcon: TCon, src: FilePositional)
		extends PositionedError("Type constructor '" + tcon.c.id + "' is being applied with too many type arguments.", src)
