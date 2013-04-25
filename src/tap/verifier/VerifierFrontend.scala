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

        val (ord, defs, types, subst) = ProgramVerifier(asts)

        val tci = new TypeclassInlining(defs, types, subst)
        tci()
        (ord, defs, types)

        // TODO: quals in forall.
        // TODO much later: GADTs lol
    }
}
