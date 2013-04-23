package tap.verifier

import tap.ModuleId
import tap.ast._
import tap.ast.parser.SExpressionParser
import tap.ir.TapNode
import tap.types.Type
import tap.types.classes.Qual
import tap.types.inference.TypeInference.ExprTypeMap
import tap.types.inference.Substitutions.Subst
import tap.util.{Graph, trace}
import tap.verifier.defs._
import tap.verifier.errors._
import tools.nsc.io.File

object VerifierFrontend {

    def apply (files: Array[File]): (Seq[Seq[String]], ModuleDefinitions, ExprTypeMap) = {

        // Parse each file to aquire ASTs and store any modules found
        val asts = files.foldLeft(Map.empty[String, ASTModule]) { case (asts, file) =>
            SExpressionParser(file.toString(), file.slurp()) match {
                case ast @ ASTModule(name, _, _, _, _, _, _, _) =>
                    if (asts contains name) throw DuplicateModuleError(name)
                    asts + (name -> ast)
            }
        }

        // Extract the imported dependencies for each module. Also check that the dependencies actually exist in the
        // imported modules. Also ensure any exported modules are in the import list.
        val deps = asts.valuesIterator.foldLeft(Map.empty[String, Seq[String]]) {
            case (deps, ast @ ASTModule(name, exports, _, _, _, _, _, _)) =>
                val imports = findImports(ast)
                val missingImports = imports filterNot { asts contains _ }
                if (missingImports.nonEmpty) throw ModuleMissingImportsError(name, missingImports)
                val missingExports = exports collect { case ExModule(id) if !(imports contains id) => id }
                if (missingExports.nonEmpty) throw ExportModuleWithoutImportError(name, missingExports)
                deps + (name -> imports)
        }

        // Sort and modules into dependency order, grouping together modules that have circular dependencies
        val ord = Graph.components(deps)

        // Construct the lookup table each module for resolving fully qualified ids from local ids
        val scopeMaps = deps map { case (mId, imports) =>
            // TODO: detect name collisions within imports/module and throw errors
            // TODO: allow qualified importing  } needs support in the AST
            // TODO: allow partial importing    } needs support in the AST
            mId -> imports.foldLeft(findExportedDefinitions(mId, asts, ImportedDefinitions.empty, Set(mId))) {
                case (defs, i) => findExportedDefinitions(i, asts, defs, Set(i))
            }
        }

        // Make a list of all the dependencies of each module, so when A imports B imports C, A's dependencies include
        // both B and C. This is used to track the reach of instances - they are not explicitly imported, they just
        // "leak" out of every module they're defined in or imported into, all the way down the heirarchy
        def accDeps(mId: String, deps: Map[String, Seq[String]], seen: Set[String]): Set[String] = {
            if (deps contains mId) (deps(mId) flatMap {
                case dep if !(seen contains dep) => accDeps(dep, deps, seen + dep)
                case _ => Set.empty[String]
            }).toSet + mId
            else Set(mId)
        }
        val explModuleDependencies = (deps map { case (mId, mDeps) =>
            mId -> (mDeps flatMap { dep => accDeps(dep, deps, Set(dep)) }).toSet
        }).toMap

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

        val tci = new TypeclassInlining(defs._1, defs._2, defs._3)
        tci()
        (ord, defs._1, defs._2)

        // TODO: quals in forall.
        // TODO much later: GADTs lol
    }

    def findExportedDefinitions(mId: String, asts: Map[String, ASTModule], defs: ImportedDefinitions, seen: Set[String]): ImportedDefinitions = {
        asts(mId).exports.foldLeft(defs) {
            case (defs, ExDataType(id, dcons)) =>
                dcons.foldLeft(defs.addTCon(id, ModuleId(mId, id))) { (defs, id) =>
                    defs.addDCon(id, ModuleId(mId, id))
                }
            case (defs, ExClass(id)) => defs.addClass(id, ModuleId(mId, id))
            case (defs, ExMember(id)) => defs.addMember(id, ModuleId(mId, id))
            case (defs, ExModule(id)) if !(seen contains id) => findExportedDefinitions(id, asts, defs, seen + id)
            case _ => defs
        }
    }

    /**
     * Finds named imports within a module.
     */
    def findImports(module: ASTModule) = ModuleVerifier.findImports(module)
}
