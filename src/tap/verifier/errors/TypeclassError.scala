package tap.verifier.errors

import tap.ModuleId
import tap.ast.FilePositional
import tap.types.Type
import tap.types.classes.TypeclassDef
import tap.util.PrettyPrint._

// ---[ references ]-----------------------------------------------------------

case class TypeclassArityError(id: String, required: Int, actual: Int, src: FilePositional)
		extends PositionedError("Typeclass '" + id + "' requires " + required + " arguments, acutal: " + actual, src)

case class TypeclassIllegalParameterError(msg: String, src: FilePositional)
		extends PositionedError(msg, src)

// ---[ definitions ]----------------------------------------------------------

case class TypeclassDuplicateMemberDefinitionError(id: ModuleId, memn: String, src: FilePositional)
		extends PositionedError("Typeclass '" + id.id + "' has duplicate definition for member '" + memn + "'", src)

case class TypeclassDuplicateMemberImplementationError(id: ModuleId, memn: String, src: FilePositional)
		extends PositionedError("Typeclass '" + id.id + "' has duplicate implementation for member '" + memn + "'", src)

case class TypeclassImplementsUnknownMemberError(id: ModuleId, memn: String, src: FilePositional)
		extends PositionedError("Typeclass '" + id.id + "' implements member '" + memn + "' but has no type declaration", src)

case class TypeclassIllegalMemberDefinition(id: ModuleId, memn: String, src: FilePositional)
		extends PositionedError("Typeclass '" + id.id + "' defines member '" + memn + "' with an illegal type (the set of class type variables are not reachable)", src)

// ---[ instances ]------------------------------------------------------------

case class InstanceIncompleteError(tc: TypeclassDef, ps: Seq[Type], missingMembers: Iterable[String], src: FilePositional)
		extends PositionedError("Typeclass instance '" + tc.name + " (" + ps.map(prettyPrint).mkString(", ") + ")' implementation is incomplete, requires: '" + missingMembers.mkString("', '") + "'", src)

case class InstanceDuplicateMemberError(tc: TypeclassDef, ps: Seq[Type], member: String, src: FilePositional)
		extends PositionedError("Typeclass instance '" + tc.name + " (" + ps.map(prettyPrint).mkString(", ") + ")' has duplicate implementation for member '" + member + "'", src)

case class InstanceUnknownMemberError(tc: TypeclassDef, ps: Seq[Type], member: String, src: FilePositional)
		extends PositionedError("Typeclass instance '" + tc.name + " (" + ps.map(prettyPrint).mkString(", ") + ")' implements member '" + member + "' that is not defined in the typeclass", src)