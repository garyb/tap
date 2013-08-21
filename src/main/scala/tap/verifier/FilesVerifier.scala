package tap.verifier

import tap.ast._
import tap.ast.parser.SExpressionParser
import tap.types.inference.TypeInference.ExprTypeMap
import tap.verifier.defs._
import tap.verifier.errors._
import java.io.File
import scala.io.Source

object FilesVerifier {

    def apply (files: Array[File]): (Seq[Seq[String]], ModuleDefinitions, ExprTypeMap) = {

        // Parse each file to aquire ASTs and store any modules found
        val asts = files.foldLeft(Map.empty[String, ASTModule]) { case (asts, file) =>
            SExpressionParser(file.getName, Source.fromFile(file).mkString) match {
                case ast @ ASTModule(name, _) =>
                    if (asts contains name) throw DuplicateModuleError(name)
                    asts + (name -> ast)
            }
        }

        val (env, ord, defs) = ProgramVerifier(asts)

        val tci = new TypeclassInlining(defs, env)
        val (env1, _) = tci()
        (ord, defs, env1.ets)
    }
}
