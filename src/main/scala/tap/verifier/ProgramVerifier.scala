package tap.verifier

import defs.{ModuleDefinitions, DefinitionsLookup}
import tap.verifier.errors._
import tap.util.{trace, Graph}
import tap.ir.TapNode
import tap.types.classes.Qual
import tap.types.Type
import tap.types.inference.Substitutions._
import tap.ast._
import tap.ModuleId
import tap.ast.ASTDataTypeExport
import tap.ast.ASTMemberExport
import language.reflectiveCalls
import tap.ast.ASTModule
import tap.ast.ASTDataType
import tap.verifier.errors.ExportModuleWithoutImportError
import tap.verifier.errors.ModuleSelfImportError
import tap.ModuleId
import tap.ast.ASTDataTypeExport
import scala.Some
import tap.verifier.errors.HidingImportError
import tap.ast.ASTImport
import tap.ast.ASTClassExport
import tap.ast.ASTMemberExport
import tap.ast.ASTDef
import tap.ast.ASTModuleExport
import tap.verifier.errors.ModuleMissingImportsError
import tap.ast.ASTClass
import tap.ast.ASTLet

object ProgramVerifier {

    type Modules = Map[String, ASTModule]
    type Imports = Map[String, Set[ASTImport]]

    def apply(asts: Modules): (List[List[String]], ModuleDefinitions, Map[TapNode, Qual[Type]], Subst) = {

        val imports = asts mapValues findImports
        val deps = findModuleDependencies(asts, imports)
        val scopeMaps = makeScopedLookups(asts, imports)

        val explModuleDependencies = (deps map { case (mId, mDeps) =>
            mId -> mDeps.flatMap { dep => Graph.successors(dep, deps) }
        }).toMap

        // Sort and modules into dependency order, grouping together modules that have circular dependencies
        val ord = Graph.components(deps)

        // Iterate through the grouped modules and run the verifier on each group, building up the list of all
        // module-scoped definitions in the program
        val defs = ord.foldLeft((ModuleDefinitions.defaults, Map.empty[TapNode, Qual[Type]], Map.empty: Subst)) {
            case ((defs0, ets0, s0), moduleGroup) =>
                if (moduleGroup.length == 1) trace("Resolving module", moduleGroup(0))
                else trace("Resolving module group", moduleGroup.mkString(", "))
                val scopeMap = (moduleGroup map { id => id -> scopeMaps(id) }).toMap
                val moduleDeps = (moduleGroup map { id => id -> (explModuleDependencies(id) ++ moduleGroup) }).toMap
                val modules = moduleGroup map { id => asts(id) }
                val verifier = new ModuleVerifier(scopeMap)
                val typechecker = new ModuleTypeInference(modules, scopeMap, moduleDeps)
                val (defs1, s1, ets1) = typechecker(verifier(modules, defs0))
                (defs1, ets0 ++ ets1, s0 ++ s1)
        }

        (ord, defs._1, defs._2, defs._3)
    }

    /**
     * Finds named imports within a module.
     */
    def findImports(module: ASTModule): Set[ASTImport] = {
        val imports = module.members.collect { case i: ASTImport => i }.toSet
        if (imports exists { i => i.module == module.name }) throw ModuleSelfImportError(module.name)
        if (module.name == "Prelude") imports else imports + ASTImport("Prelude", None, None)
    }

    /**
     * Extract the imported dependencies for each module. Also check that the dependencies actually exist in the
     * imported modules. Also ensure any exported modules are in the import list.
     */
    def findModuleDependencies(asts: Modules, imports: Imports): Map[String, Set[String]] = {
        asts map { case (name, ast) =>
            val importNames = imports(name) map { i => i.module }
            val missingImports = importNames filterNot { asts contains _ }
            if (missingImports.nonEmpty) throw ModuleMissingImportsError(name, missingImports)
            val missingExports = ast.members collect {
                case ASTModuleExport(name) if !(importNames contains name) => name
            }
            if (missingExports.nonEmpty) throw ExportModuleWithoutImportError(name, missingExports)
            name -> importNames
        }
    }

    /**
     * Constructs a lookup table for each module for resolving fully qualified IDs from module-local IDs.
     * TODO: refactor somehow, this is pretty awful
     */
    def makeScopedLookups(asts: Modules, imports: Imports): Map[String, DefinitionsLookup] = {
        val lookups = asts mapValues { m => findExportedDefinitions(m.name, asts) }
        asts map { case (_, ast @ ASTModule(mId, defs)) =>

            val importedDefs = imports(mId).foldLeft(DefinitionsLookup.defaults) {
                case (importedDefs, ast @ ASTImport(name, defs, prefix)) =>
                    var lookup = lookups.getOrElse(name, throw UnknownModuleError(name, ast))
                    if (defs != None) lookup = lookup.select(mId, name, defs.get)
                    if (prefix != None) lookup = lookup.addPrefix(prefix.get)
                    DefinitionsLookup.merge(mId, importedDefs, lookup)
            }

            def getModuleId(defType: String, coll: Map[String, ModuleId])(thing: { def name: String }) = {
                coll.get(thing.name) match {
                    // The guard is needed here because sometimes a module is inadvertantly importing itself - e.g. any
                    // of the modules that Prelude exports will always have this problem, as those modules also import
                    // Prelude themselves
                    case Some(id) if id.mId != mId => throw HidingImportError(mId, defType, thing.name, id, thing.asInstanceOf[FilePositional])
                    case _ => thing.name -> ModuleId(mId, thing.name)
                }
            }

            val dtDefs = defs collect { case dtd: ASTDataType => dtd }
            val tconDefs = (defs collect { case dtd: ASTDataType => dtd.constructors }).flatten
            val tcDefs = defs collect { case tc: ASTClass => tc }
            val memberDefs = defs collect { case m: ASTDef => m }
            val memberImpls = defs collect { case m: ASTLet => m }
            val tcMemberDefs = tcDefs flatMap { tc => tc.members }

            val tcons = (dtDefs map getModuleId("type constructor", importedDefs.tcons)).toMap
            val dcons = (tconDefs map getModuleId("data constructor", importedDefs.dcons)).toMap
            val tcs = (tcDefs map getModuleId("typeclass", importedDefs.tcs)).toMap
            val mms =
                (memberDefs map getModuleId("member", importedDefs.members)).toMap ++
                (memberImpls map getModuleId("member", importedDefs.members)) ++
                (tcMemberDefs map getModuleId("member", importedDefs.members))

            mId -> DefinitionsLookup.merge(mId, importedDefs, DefinitionsLookup(tcons, dcons, tcs, mms))
        }
    }

    /**
     * Finds all the definitions exported from a module. If module A exports module B, the resulting list will contain
     * all the exported definitions from both A and B.
     */
    def findExportedDefinitions(mId: String, asts: Modules): DefinitionsLookup = {
        def find(mId: String, seen: Set[String]): DefinitionsLookup = {
            val explicitExports = asts(mId).members.foldLeft(DefinitionsLookup.empty) {
                case (defs, ASTDataTypeExport(id, dcons)) =>
                    dcons.foldLeft(defs.addTCon(id, ModuleId(mId, id))) {
                        (defs, id) => defs.addDCon(id, ModuleId(mId, id))
                    }
                case (defs, ASTClassExport(id)) => defs.addClass(id, ModuleId(mId, id))
                case (defs, ASTMemberExport(id)) => defs.addMember(id, ModuleId(mId, id))
                case (defs, _) => defs
            }
            val exports =
                if (explicitExports != DefinitionsLookup.empty) explicitExports
                else asts(mId).members.foldLeft(DefinitionsLookup.empty) {
                    case (defs, ASTDataType(id, _, dcons)) =>
                        dcons.foldLeft(defs.addTCon(id, ModuleId(mId, id))) {
                            case (defs, dcon) => defs.addDCon(dcon.name, ModuleId(mId, dcon.name))
                        }
                    case (defs, ASTClass(id, _, _, members)) =>
                        members.foldLeft(defs.addClass(id, ModuleId(mId, id))) {
                            case (defs, m) => defs.addMember(m.name, ModuleId(mId, m.name))
                        }
                    case (defs, ASTDef(id, _)) => defs.addMember(id, ModuleId(mId, id))
                    case (defs, ASTLet(id, _)) => defs.addMember(id, ModuleId(mId, id))
                    case (defs, _) => defs
                }
            asts(mId).members.foldLeft(exports) {
                case (defs, ASTModuleExport(id)) =>
                    if (seen contains id) defs
                    else {
                        // "unsafe" merge here is okay because we check for import collisions in `makeScopedLookups`
                        val idefs = find(id, seen + id)
                        DefinitionsLookup(idefs.tcons ++ defs.tcons,
                            idefs.dcons ++ defs.dcons,
                            idefs.tcs ++ defs.tcs,
                            idefs.members ++ defs.members)
                    }
                case (defs, _) => defs
            }
        }
        find(mId, Set(mId))
    }
}
