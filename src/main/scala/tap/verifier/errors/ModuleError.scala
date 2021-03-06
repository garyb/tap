package tap.verifier.errors

import tap.util.PrettyPrint._
import tap.Id
import tap.ast.FilePositional

// ---[ general ]--------------------------------------------------------------

case class DuplicateModuleError(mId: String)
        extends VerifierError("Module '" + mId + "' was defined multiple times in the current classpath")

// ---[ imports & exports ]----------------------------------------------------

case class ModuleMissingImportsError(mId: String, imports: Iterable[String])
        extends VerifierError("Module '" + mId + "' attempted to import missing module" + (if (imports.size == 1) "" else "s") + ": '" + imports.mkString("', '") + "'")

case class ExportModuleWithoutImportError(mId: String, modules: Iterable[String])
        extends VerifierError("Module " + mId + " exports module" + (if (modules.size > 1) "s " + modules.mkString(", ") else " " + modules.head) + " but does not import " + (if (modules.size > 1) "them" else "it"))

// ---[ members ]--------------------------------------------------------------

case class ModuleMemberInitCycleError(ids: Seq[Id])
        extends VerifierError("Cycle found in member initialisation: " + (ids map prettyPrint mkString ", "))

case class ModuleMemberInitRecursiveError(id: Id)
        extends VerifierError("Recursion found in member initialisation: " + prettyPrint(id))

case class ModuleMissingImplementationError(mId: String, missing: String, src: FilePositional)
        extends PositionedError("Module '" + mId + "' defines '" + missing + "' but provides no implementation", src)

case class ModuleDuplicateDefinition(mId: String, thing: String, member: String, src: FilePositional)
        extends PositionedError("Module '" + mId + "' has duplicate definition for " + thing + " '" + member + "'", src)
