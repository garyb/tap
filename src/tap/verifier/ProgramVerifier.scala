package tap.verifier

import defs.{ModuleDefinitions, DefinitionsLookup}
import errors.{HidingImportError, ExportModuleWithoutImportError, ModuleMissingImportsError}
import tap.util.{trace, Graph}
import tap.ir.TapNode
import tap.types.classes.Qual
import tap.types.Type
import tap.types.inference.Substitutions._
import tap.ast._
import tap.ModuleId
import tap.ast.ExDataType
import tap.ast.ExMember
import language.reflectiveCalls

object ProgramVerifier {

    def apply(asts: Map[String, ASTModule]): (List[List[String]], ModuleDefinitions, Map[TapNode, Qual[Type]], Subst) = {

        val deps = findModuleDependencies(asts)

        val scopeMaps = makeScopedLookups(asts, deps)

        val explModuleDependencies = (deps map { case (mId, mDeps) =>
            mId -> (mDeps flatMap { dep => accumulateDependencies(dep, deps, Set(dep)) }).toSet
        }).toMap

        // Sort and modules into dependency order, grouping together modules that have circular dependencies
        val ord = Graph.components(deps)

        // Iterate through the grouped modules and run the verifier on each group, building up the list of all
        // module-scoped definitions in the program
        val defs = ord.foldLeft((ModuleDefinitions.empty, Map.empty[TapNode, Qual[Type]], Map.empty: Subst)) { case ((defs0, ets0, s0), moduleGroup) =>
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
     * Extract the imported dependencies for each module. Also check that the dependencies actually exist in the imported modules. Also ensure any exported modules are in the import list.
     * @param asts
     */
    def findModuleDependencies(asts: Map[String, ASTModule]): Map[String, Set[String]] = asts mapValues {
        case ast @ ASTModule(name, exports, _, _, _, _, _, _) =>
            val imports = findImportedModules(ast)
            val missingImports = imports filterNot { asts contains _ }
            if (missingImports.nonEmpty) throw ModuleMissingImportsError(name, missingImports)
            val missingExports = exports collect { case ExModule(id) if !(imports contains id) => id }
            if (missingExports.nonEmpty) throw ExportModuleWithoutImportError(name, missingExports)
            imports
    }

    /**
     * Constructs a lookup table for each module for resolving fully qualified IDs from module-local IDs.
     */
    def makeScopedLookups(asts: Map[String, ASTModule], deps: Map[String, Set[String]]): Map[String, DefinitionsLookup] =
        deps map { case (mId, imports) =>
            // TODO: allow qualified importing  } needs support in the AST
            // TODO: allow partial importing    } needs support in the AST

            val importedDefs = imports.foldLeft(DefinitionsLookup.empty) {
                case (defs, i) => DefinitionsLookup.merge(mId, defs, findExportedDefinitions(i, asts, Set(mId, i)))
            }

            def getModuleId(defType: String, coll: Map[String, ModuleId])(thing: { def name: String }) = {
                coll.get(thing.name) match {
                    case Some(id) => throw HidingImportError(mId, defType, thing.name, id, thing.asInstanceOf[FilePositional])
                    case None => thing.name -> ModuleId(mId, thing.name)
                }
            }

            val module = asts(mId)
            val tcons = (module.datatypes map getModuleId("type constructor", importedDefs.tcons)).toMap
            val dcons = (module.datatypes flatMap { dtd => dtd.constructors } map getModuleId("data type", importedDefs.dcons)).toMap
            val tcs = (module.typeclasses map getModuleId("type class", importedDefs.tcs)).toMap
            val members =
                (module.memberDefs map getModuleId("member", importedDefs.members)).toMap ++
                (module.memberImpls map getModuleId("member", importedDefs.members)) ++
                (module.typeclasses flatMap { tc => tc.members } map getModuleId("typeclass member", importedDefs.members))

            val moduleDefs = DefinitionsLookup(tcons, dcons, tcs, members)
            mId -> DefinitionsLookup.merge(mId, importedDefs, moduleDefs)
        }

    /**
     * Finds the definitions exported from a module. If moudule A exports module B, the resulting list will contain all
     * the exported definitions from both A and B.
     */
    def findExportedDefinitions(mId: String, asts: Map[String, ASTModule], seen: Set[String]): DefinitionsLookup = {
        var nullDefs = DefinitionsLookup.empty
        if (mId == "Prelude") nullDefs = nullDefs.addTCon("->", ModuleId("Prelude", "->"))
        asts(mId).exports.foldLeft(nullDefs) {
            case (defs, ExDataType(id, dcons)) =>
                dcons.foldLeft(defs.addTCon(id, ModuleId(mId, id))) { (defs, id) =>
                    defs.addDCon(id, ModuleId(mId, id))
                }
            case (defs, ExClass(id)) => defs.addClass(id, ModuleId(mId, id))
            case (defs, ExMember(id)) => defs.addMember(id, ModuleId(mId, id))
            case (defs, ExModule(id)) =>
                if (seen contains id) defs
                else {
                    // TODO: this merge is unsafe, not checking for key collisions
                    val idefs = findExportedDefinitions(id, asts, seen + id)
                    DefinitionsLookup(
                        idefs.tcons ++ defs.tcons,
                        idefs.dcons ++ defs.dcons,
                        idefs.tcs ++ defs.tcs,
                        idefs.members ++ defs.members)
                }
        }
    }

    /**
     * Make a list of all the dependencies of each module, so when A imports B imports C, A's dependencies include both
     * B and C. This is used to track the reach of instances - they are not explicitly imported, they just "leak" out
     * of every module they're defined in or imported into, all the way down the heirarchy
     */
    def accumulateDependencies(mId: String, deps: Map[String, Set[String]], seen: Set[String]): Set[String] = {
        deps.get(mId) match {
            case Some(ids) =>
                ids.foldLeft(seen + mId) { case (seen, id) =>
                    if (seen contains id) seen
                    else accumulateDependencies(id, deps, seen + id)
                }
            case None => seen + mId
        }
    }

    /**
     * Finds named imports within a module.
     */
    def findImportedModules(module: ASTModule): Set[String] = {
        val name = module.name
        val imports = module.imports filter { _ != name }
        if (name == "Prelude") imports else imports + "Prelude"
    }
}
