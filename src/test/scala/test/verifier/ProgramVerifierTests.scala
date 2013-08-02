package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.ast._
import tap.verifier.ProgramVerifier._
import tap.verifier.defs.DefinitionsLookup
import tap.ModuleId
import tap.verifier.errors._

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

    // ------------------------------------------------------------------------

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

    // ------------------------------------------------------------------------

    behavior of "makeScopedLookups"

    it should "throw an error if a module imports from a non-existant module" in {
        val module = ASTModule("Test", List(ASTImport("Foo", Some(Set("thingy")), None)))
        val imports = findImports(module)
        evaluating {
            makeScopedLookups(Map("Test" -> module), Map("Test" -> imports))
        } should produce [UnknownModuleError]
    }

    it should "throw an error if a module imports a non-existant definition from another module" in {
        val moduleA = ASTModule("Test", List(ASTImport("Foo", Some(Set("thingy")), None)))
        val moduleB = ASTModule("Foo", Nil)
        val modules = Map("Test" -> moduleA, "Foo" -> moduleB)
        val imports = modules mapValues findImports
        evaluating { makeScopedLookups(modules, imports) } should produce [UnknownImportDefsError]
    }

    ignore should "throw an error if a module has conflicting imports for a type constructor" in {}
    ignore should "throw an error if a module has conflicting imports for a data constructor" in {}
    ignore should "throw an error if a module has conflicting imports for a typeclass" in {}

    it should "throw an error if a module has conflicting imports for a member" in {
        val moduleA = ASTModule("Test", List(ASTImport("FooA", None, None), ASTImport("FooB", None, None)))
        val moduleB = ASTModule("FooA", List(ASTLet("fn", ASTFunction(List("a"), ASTValueRead("a")))))
        val moduleC = ASTModule("FooB", List(ASTLet("fn", ASTFunction(List("a"), ASTValueRead("a")))))
        val modules = Map("Test" -> moduleA, "FooA" -> moduleB, "FooB" -> moduleC)
        val imports = modules mapValues findImports
        evaluating { makeScopedLookups(modules, imports) } should produce [ImportConflictError]
    }

    ignore should "make scope maps" in {}

    // ------------------------------------------------------------------------

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

    it should "export every item in the module if no explicit exports are defined" in {
        val asts = Map(
            "Test" -> ASTModule("Test", List(
                ASTDataType("TestType", Nil, List(ASTDataCon("TestType", Nil))),
                ASTClass("TestClass", Nil, Nil, List(
                    ASTClassMemberDef("TestClassMemberD", ASTQType(Nil, ASTTypeCon("Bool"))),
                    ASTClassMemberImpl("TestClassMemberI", ASTValueRead("True")))),
                ASTLet("TestMember1", ASTValueRead("True")),
                ASTDef("TestMember2", ASTQType(Nil, ASTTypeCon("Bool"))))))
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
