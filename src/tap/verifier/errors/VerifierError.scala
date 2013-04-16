package tap.verifier.errors

import tap.ast.FilePositional

// ---[ error bases ]----------------------------------------------------------

abstract class VerifierError(msg: String)
		extends Error(msg)

abstract class PositionedError(msg: String, src: FilePositional)
		extends VerifierError(msg + " (" + src.file + ", line " + src.pos.line + ", col " + src.pos.column + ")")

// ---[ generic errors ]-------------------------------------------------------

case class VerifierMiscError(msg: String, src: FilePositional)
		extends PositionedError("Syntax error: " + msg, src)

case class SyntaxError(msg: String, src: FilePositional)
		extends PositionedError("Syntax error: " + msg, src)

case class DuplicateDefinitionError(thing: String, name: String, src: FilePositional)
		extends PositionedError("Duplicate definition of " + thing + " '" + name + "'", src)

case class NamespaceError(thing: String, name: String, src: FilePositional)
		extends PositionedError(thing + " '" + name + "' is already present in the current namespace", src)

// ---[ unknown references ]---------------------------------------------------

case class MissingDefinitionError(ident: String, src: FilePositional)
		extends PositionedError("Reference to undeclared value, function, or data constructor '" + ident + "'", src)

case class MissingDataConstructorError(ident: String, src: FilePositional)
		extends PositionedError("Unknown data constructor '" + ident + "'", src)











