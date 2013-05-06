package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.ast._
import tap.verifier.ProgramVerifier._
import tap.verifier.defs.DefinitionsLookup
import tap.ModuleId
import tap.verifier.errors.{ExportModuleWithoutImportError, ModuleMissingImportsError, ModuleSelfImportError}

class ProgramVerifierTests extends FlatSpec with GivenWhenThen {

    behavior of "findImports"

    it should "automatically include a reference to the Prelude module" in {
        val module = ASTModule("Test", Nil)
        findImports(module) should be === Set(ASTImport("Prelude", None, None))
    }

    it should "exclude the auto-Prelude-import when the current module is Prelude" in {
        val module = ASTModule("Prelude", Nil)
        findImports(module) should be === Set.empty
    }

    it should "throw an error if a module imports itself" in {

        Given("a basic self-import")
        evaluating {
            findImports(ASTModule("Test", List(ASTImport("Test", None, None))))
        } should produce [ModuleSelfImportError]

        Given("a prefixed self-import")
        evaluating {
            findImports(ASTModule("Test", List(ASTImport("Test", None, Some("AltName")))))
        } should produce [ModuleSelfImportError]
    }

    it should "find referenced modules" in {
        val module = ASTModule("Test", List(ASTImport("Foo", None, None), ASTImport("Bar", None, None), ASTImport("Baz", None, None)))
        findImports(module) should be === Set(ASTImport("Prelude", None, None), ASTImport("Foo", None, None), ASTImport("Bar", None, None), ASTImport("Baz", None, None))
    }

    behavior of "findModuleDependencies"

    it should "build a map of module dependencies" in {

        val asts = Map(
            "TestA" -> ASTModule("TestA", List(ASTImport("Prelude", None, None), ASTImport("TestB", None, None))),
            "TestB" -> ASTModule("TestB", List(ASTImport("Prelude", None, None), ASTImport("TestC", None, None), ASTImport("TestD", None, None))),
            "TestC" -> ASTModule("TestC", List(ASTImport("Prelude", None, None))),
            "TestD" -> ASTModule("TestD", List(ASTImport("Prelude", None, None))),
            "Prelude" -> ASTModule("Prelude", Nil)
        )

        findModuleDependencies(asts, asts mapValues findImports) should be === Map(
            "TestA" -> Set("Prelude", "TestB"),
            "TestB" -> Set("Prelude", "TestC", "TestD"),
            "TestC" -> Set("Prelude"),
            "TestD" -> Set("Prelude"),
            "Prelude" -> Set()
        )
    }

    it should "throw an error if an import is missing from the modules map" in {
        val asts = Map("TestA" -> ASTModule("TestA", List(ASTImport("Prelude", None, None))))
        evaluating {
            findModuleDependencies(asts, asts mapValues findImports)
        } should produce [ModuleMissingImportsError]
    }

    it should "throw an error if a module exports a module it is not importing" in {
        val asts = Map(
            "Prelude" -> ASTModule("Prelude", Nil),
            "TestA" -> ASTModule("TestA", List(ASTImport("Prelude", None, None))),
            "TestB" -> ASTModule("TestB", List(ASTImport("Prelude", None, None), ASTModuleExport("TestA")))
        )
        evaluating {
            findModuleDependencies(asts, asts mapValues findImports)
        } should produce [ExportModuleWithoutImportError]
    }

    behavior of "makeScopedLookups"

    ignore should "make scope maps" in {}
    ignore should "throw an error if a module imports a non existant named definition from another module" in {}

    behavior of "findExportedDefinitions"

    it should "collect all exported definitions for a module" in {
        val asts = Map(
            "TestA" -> ASTModule("TestA", List(
                ASTDataTypeExport("TestType", Set("TestType")),
                ASTClassExport("TestClass"),
                ASTMemberExport("TestX")
            )))

        findExportedDefinitions("TestA", asts) should be ===
            DefinitionsLookup(
                Map("TestType" -> ModuleId("TestA", "TestType")),
                Map("TestType" -> ModuleId("TestA", "TestType")),
                Map("TestClass" -> ModuleId("TestA", "TestClass")),
                Map("TestX" -> ModuleId("TestA", "TestX")))
    }

    it should "also collect all exported definitions for any exported modules" in {
        val asts = Map(
            "TestA" -> ASTModule("TestA", List(
                ASTDataTypeExport("TestType", Set("TestType")),
                ASTClassExport("TestClass"),
                ASTMemberExport("TestX"),
                ASTModuleExport("TestB")
            )),
            "TestB" -> ASTModule("TestB", List(
                ASTDataTypeExport("TestType2", Set("TestType2")),
                ASTClassExport("TestClass2"),
                ASTMemberExport("TestX2")
            )))

        findExportedDefinitions("TestA", asts) should be ===
                DefinitionsLookup(
                    Map("TestType" -> ModuleId("TestA", "TestType"),
                        "TestType2" -> ModuleId("TestB", "TestType2")),
                    Map("TestType" -> ModuleId("TestA", "TestType"),
                        "TestType2" -> ModuleId("TestB", "TestType2")),
                    Map("TestClass" -> ModuleId("TestA", "TestClass"),
                        "TestClass2" -> ModuleId("TestB", "TestClass2")),
                    Map("TestX" -> ModuleId("TestA", "TestX"),
                        "TestX2" -> ModuleId("TestB", "TestX2")))
    }

    it should "export ever item in the module if no explicit exports are defined" in {
        val asts = Map(
            "Test" -> ASTModule("Test", List(
                ASTDataTypeDefinition("TestType", Nil, List(ASTDataTypeConstructor("TestType", Nil))),
                ASTTypeClassDefinition("TestClass", Nil, Nil, List(
                    ASTTypeClassMemberDefinition("TestClassMemberD", Nil, ASTTypeCon("Bool")),
                    ASTTypeClassMemberImplementation("TestClassMemberI", ASTValueRead("True")))),
                ASTLet("TestMember1", ASTValueRead("True")),
                ASTDef("TestMember2", Nil, ASTTypeCon("Bool")))))
        findExportedDefinitions("Test", asts) should be ===
                DefinitionsLookup(
                    Map("TestType" -> ModuleId("Test", "TestType")),
                    Map("TestType" -> ModuleId("Test", "TestType")),
                    Map("TestClass" -> ModuleId("Test", "TestClass")),
                    Map("TestClassMemberD" -> ModuleId("Test", "TestClassMemberD"),
                        "TestClassMemberI" -> ModuleId("Test", "TestClassMemberI"),
                        "TestMember1" -> ModuleId("Test", "TestMember1"),
                        "TestMember2" -> ModuleId("Test", "TestMember2")))
    }
}
