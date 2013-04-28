package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.ast._
import tap.verifier.ProgramVerifier._
import tap.verifier.defs.DefinitionsLookup
import tap.ModuleId
import tap.verifier.errors.{ExportModuleWithoutImportError, ModuleMissingImportsError, ModuleSelfImportError}

class ProgramVerifierTests extends FlatSpec with GivenWhenThen {

    behavior of "findModuleDependencies"

    it should "build a map of module dependencies" in {

        /*val asts = Map(
            "TestA" -> ASTModule("TestA", Set.empty, Set(ASTImport("Prelude", None, None), ASTImport("TestB", None, None)), Nil, Nil, Nil, Nil, Nil),
            "TestB" -> ASTModule("TestB", Set.empty, Set(ASTImport("Prelude", None, None), ASTImport("TestC", None, None), ASTImport("TestD", None, None)), Nil, Nil, Nil, Nil, Nil),
            "TestC" -> ASTModule("TestC", Set.empty, Set(ASTImport("Prelude", None, None)), Nil, Nil, Nil, Nil, Nil),
            "TestD" -> ASTModule("TestD", Set.empty, Set(ASTImport("Prelude", None, None)), Nil, Nil, Nil, Nil, Nil),
            "Prelude" -> ASTModule("Prelude", Set.empty, Set.empty, Nil, Nil, Nil, Nil, Nil)
        )

        findModuleDependencies(asts) should be === Map(
            "TestA" -> Set("Prelude", "TestB"),
            "TestB" -> Set("Prelude", "TestC", "TestD"),
            "TestC" -> Set("Prelude"),
            "TestD" -> Set("Prelude"),
            "Prelude" -> Set()
        )*/
    }

    it should "throw an error if an import is missing from the modules map" in {
        val asts = Map("TestA" -> ASTModule("TestA", Set.empty, Set(ASTImport("Prelude", None, None)), Nil, Nil, Nil, Nil, Nil))
        evaluating { findModuleDependencies(asts) } should produce [ModuleMissingImportsError]
    }

    it should "throw an error if a module exports a module it is not importing" in {
        val asts = Map("TestA" -> ASTModule("TestA", Set(ExModule("TestB")), Set(ASTImport("Prelude", None, None)), Nil, Nil, Nil, Nil, Nil))
        evaluating { findModuleDependencies(asts) } should produce [ExportModuleWithoutImportError]
    }

    /*behavior of "makeScopedLookups"

    ignore should "make scope maps" in {}
    ignore should "throw an error if a module imports a non existant named definition from another module" in {}

    behavior of "findExportedDefinitions"

    it should "collect all exported definitions for a module" in {
        val asts = Map("TestA" -> ASTModule("TestA",
            Set(
                ExDataType("TestType", Set("TestType")),
                ExClass("TestClass"),
                ExMember("TestX")
            ),
            Set.empty, Nil, Nil, Nil, Nil, Nil))

        findExportedDefinitions("TestA", asts, Set.empty) should be ===
            DefinitionsLookup(
                Map("TestType" -> ModuleId("TestA", "TestType")),
                Map("TestType" -> ModuleId("TestA", "TestType")),
                Map("TestClass" -> ModuleId("TestA", "TestClass")),
                Map("TestX" -> ModuleId("TestA", "TestX")))
    }

    it should "also collect all exported definitions for any exported modules" in {
        val asts = Map("TestA" -> ASTModule("TestA",
            Set(
                ExDataType("TestType", Set("TestType")),
                ExClass("TestClass"),
                ExMember("TestX"),
                ExModule("TestB")
            ),
            Set.empty, Nil, Nil, Nil, Nil, Nil),
            "TestB" -> ASTModule("TestB",
                Set(
                    ExDataType("TestType2", Set("TestType2")),
                    ExClass("TestClass2"),
                    ExMember("TestX2")
                ),
                Set.empty, Nil, Nil, Nil, Nil, Nil))

        findExportedDefinitions("TestA", asts, Set("TestA")) should be ===
                DefinitionsLookup(
                    Map("TestType" -> ModuleId("TestA", "TestType"), "TestType2" -> ModuleId("TestB", "TestType2")),
                    Map("TestType" -> ModuleId("TestA", "TestType"), "TestType2" -> ModuleId("TestB", "TestType2")),
                    Map("TestClass" -> ModuleId("TestA", "TestClass"), "TestClass2" -> ModuleId("TestB", "TestClass2")),
                    Map("TestX" -> ModuleId("TestA", "TestX"), "TestX2" -> ModuleId("TestB", "TestX2")))
    }

    behavior of "accumulateDependencies"

    ignore should "collect all dependencies and inherited dependencies for the specified module"

    behavior of "findImportedModules"

    it should "automatically include a reference to the Prelude module" in {
        val module = ASTModule("Test", Set.empty, Set.empty, Nil, Nil, Nil, Nil, Nil)
        findImportedModules(module) should be === Set("Prelude")
    }

    it should "exclude the auto-Prelude-import when the current module is Prelude" in {
        val module = ASTModule("Prelude", Set.empty, Set.empty, Nil, Nil, Nil, Nil, Nil)
        findImportedModules(module) should be === Set.empty
    }

    it should "find referenced modules" in {
        val module = ASTModule("Test", Set.empty, Set(ASTImport("Foo", None, None), ASTImport("Bar", None, None), ASTImport("Baz", None, None)), Nil, Nil, Nil, Nil, Nil)
        findImportedModules(module) should be === Set("Foo", "Bar", "Baz", "Prelude")
    }

    it should "throw an error if a module imports itself" in {

        evaluating {
            val module = ASTModule("Test", Set.empty, Set(ASTImport("Test", None, None)), Nil, Nil, Nil, Nil, Nil)
            findImportedModules(module) should be === Set("Prelude")
        } should produce [ModuleSelfImportError]

        evaluating {
            val module = ASTModule("Test", Set.empty, Set(ASTImport("Test", None, Some("AltName"))), Nil, Nil, Nil, Nil, Nil)
            findImportedModules(module) should be === Set("Prelude")
        } should produce [ModuleSelfImportError]
    }*/
}
