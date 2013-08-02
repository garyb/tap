package tap.verifier.errors

import tap.Id
import tap.ast.FilePositional
import tap.util.PrettyPrint._

case class NamespaceError(thing: String, name: String, src: FilePositional)
    extends PositionedError(thing + " '" + name + "' is already present in the current namespace", src)

case class ModuleSelfImportError(mId: String)
    extends VerifierError("Module '" + mId + "' imports itself")

case class ImportConflictError(mId: String, defType: String, localId: String, id1: Id, id2: Id)
    extends VerifierError("Module '" + mId + "' imports conflicting definitions for " + defType + " '" + localId + "': " + prettyPrint(id1) + ", " + prettyPrint(id2))

case class HidingImportError(mId: String, defType: String, id: String, iId: Id, src: FilePositional)
    extends PositionedError("Module '" + mId + "' declares " + defType + " '" + id + "' that conflicts with imported definition '" + prettyPrint(iId) + "'", src)

case class UnknownModuleError(mId: String, src: FilePositional)
    extends PositionedError("Unknown module '" + mId + "'", src)

case class UnknownImportDefsError(inModule: String, fromModule: String, things: Iterable[String])
    extends VerifierError("Module '" + inModule + "' attempted to import nonexistant definition(s) " + (things mkString ", ") + " from '" + fromModule + "'")