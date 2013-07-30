package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.ast._
import tap.ModuleId
import tap.verifier.defs.{DefinitionsLookup, ModuleDefinitions}
import tap.verifier.ModuleVerifier
import tap.verifier.errors._
import tap.types._
import tap.types.kinds._
import tap.types.classes.{Qual, IsIn, TypeclassDef}
import tap.types.classes.ClassEnvironments.Inst
import tap.ir.ValueReadExpr
import language.reflectiveCalls

class ModuleVerifierTests extends FlatSpec with GivenWhenThen {

    val nullDefs = ModuleDefinitions(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
    val nullScopes = Map("Test" -> DefinitionsLookup.empty)
    val testDefs =
        ModuleDefinitions(
            Map(ModuleId("Test", "X") -> TCon(ModuleId("Test", "X"), Star)),
            Map(ModuleId("Test", "X") -> TCon(ModuleId("Test", "X"), Star)),
            Map(ModuleId("Test", "Y") -> TypeclassDef(ModuleId("Test", "Y"), Nil, Nil, Set.empty, Set.empty)),
            Map(ModuleId("Test", "Y") -> List(Inst("Test", Nil, IsIn(ModuleId("Test", "Y"), List(Type.tString))))),
            Map(ModuleId("Test", "z") -> Qual(Nil, TCon(ModuleId("Test", "X"), Star))),
            Map(ModuleId("Test", "z") -> ValueReadExpr(ModuleId("Test", "X"))))

    // ------------------------------------------------------------------------

    behavior of "apply"

    // ------------------------------------------------------------------------

    behavior of "addDataTypeDefs"

    it should "throw an error if the name of a type constructor conflicts with an imported definition" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup(Map("A" -> ModuleId("Prelude", "A")), Map.empty, Map.empty, Map.empty)))
        val dtd = ASTDataType("A", Nil, Nil)
        evaluating {
            v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        } should produce [NamespaceError]
    }

    it should "throw an error if the name of a data constructor conflicts with an imported definition" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup(Map.empty, Map("B" -> ModuleId("Prelude", "B")), Map.empty, Map.empty)))
        val dtd = ASTDataType("A", Nil, List(ASTDataCon("B", Nil)))
        evaluating {
            v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        } should produce [NamespaceError]
    }

    it should "throw an error if a type constructor is defined more than once in a module" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", Nil, Nil)
        evaluating {
            v.addDataTypeDefs(Seq(
                "Test" -> dtd,
                "Test" -> dtd), nullDefs)
        } should produce [ModuleDuplicateDefinition]
    }

    it should "throw an error if a data constructor is defined more than once in a module" in {

        Given("a duplicate dcon in the same definition")
        val v1 = new ModuleVerifier(nullScopes)
        evaluating {
            v1.addDataTypeDefs(Seq(
                "Test" -> ASTDataType("A", Nil, List(ASTDataCon("Aa", Nil), ASTDataCon("Aa", Nil)))
            ), nullDefs)
        } should produce [ModuleDuplicateDefinition]

        Given("a duplicate dcon in different definitions")
        val v2 = new ModuleVerifier(nullScopes)
        evaluating {
            v2.addDataTypeDefs(Seq(
                "Test" -> ASTDataType("A1", Nil, List(ASTDataCon("Aa", Nil))),
                "Test" -> ASTDataType("A2", Nil, List(ASTDataCon("Aa", Nil)))
            ), nullDefs)
        } should produce [ModuleDuplicateDefinition]
    }

    it should "handle type constructors without type variables" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", Nil, Nil)
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        defs.tcons should be === Map(ModuleId("Test", "A") -> TCon(ModuleId("Test", "A"), Star))
    }

    it should "handle type constructors with type variables" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", List("p", "q"), Nil)
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        defs.tcons should be === Map(ModuleId("Test", "A") -> TCon(ModuleId("Test", "A"), Kfun(Star, Kfun(Star, Star))))
    }

    it should "handle data constructors with no arguments" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", Nil, List(ASTDataCon("B", Nil)))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        defs.dcons should be === Map(ModuleId("Test", "B") -> TCon(ModuleId("Test", "A"), Star))
    }

    it should "handle data constructors with non-type variable arguments" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addTCon("A", ModuleId("Test", "A"))
            .addTCon("B", ModuleId("Test", "B"))))
        val dtd1 = ASTDataType("A", Nil, Nil)
        val dtd2 = ASTDataType("B", Nil, List(ASTDataCon("B", List(ASTTypeCon("A")))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd1, "Test" -> dtd2), nullDefs)
        val tA = TCon(ModuleId("Test", "A"), Star)
        val tB = TCon(ModuleId("Test", "B"), Star)
        defs.dcons should be === Map(
            ModuleId("Test", "B") -> (tA fn tB)
        )
    }

    it should "handle circular dependencies between type constructors" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addTCon("A", ModuleId("Test", "A"))
            .addTCon("B", ModuleId("Test", "B"))))
        val dtd1 = ASTDataType("A", Nil, List(ASTDataCon("A", List(ASTTypeCon("B")))))
        val dtd2 = ASTDataType("B", Nil, List(ASTDataCon("B", List(ASTTypeCon("A")))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd1, "Test" -> dtd2), nullDefs)
        val tA = TCon(ModuleId("Test", "A"), Star)
        val tB = TCon(ModuleId("Test", "B"), Star)
        defs.dcons should be === Map(
            ModuleId("Test", "A") -> (tB fn tA),
            ModuleId("Test", "B") -> (tA fn tB)
        )
    }

    it should "handle self-referential data constructors" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addTCon("A", ModuleId("Test", "A"))))
        val dtd = ASTDataType("A", Nil, List(ASTDataCon("B", List(ASTTypeCon("A")))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        val tA = TCon(ModuleId("Test", "A"), Star)
        defs.dcons should be === Map(
            ModuleId("Test", "B") -> (tA fn tA)
        )
    }

    it should "handle data constructors with quantified type variable arguments" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", List("p", "q"), List(ASTDataCon("B", List(ASTTypeVar("p"), ASTTypeVar("q")))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        val fi = Type.lastForallId
        val fa = Forall(fi, List(Star, Star), TGen(fi, 0) fn (TGen(fi, 1) fn TAp(TAp(TCon(ModuleId("Test", "A"), Kfun(Star, Kfun(Star, Star))), TGen(fi, 0)), TGen(fi, 1))))
        defs.dcons should be === Map(ModuleId("Test", "B") -> fa)
    }

    it should "infer the kind of type variables based upon their usage in data constructors" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", List("p", "q"), List(ASTDataCon("B", List(ASTTypeApply(ASTTypeVar("p"), List(ASTTypeVar("q")))))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        val fi = Type.lastForallId
        val fa = Forall(fi, List(Kfun(Star, Star), Star), TAp(TGen(fi, 0), TGen(fi, 1)) fn TAp(TAp(TCon(ModuleId("Test", "A"), Kfun(Kfun(Star, Star), Kfun(Star, Star))), TGen(fi, 0)), TGen(fi, 1)))
        defs.dcons should be === Map(ModuleId("Test", "B") -> fa)
    }

    it should "throw an error if the kind of type variables conflicts in the data constructors" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("E", List("a", "b"), List(ASTDataCon("X", List(ASTTypeApply(ASTTypeVar("a"), List(ASTTypeVar("b"))))), ASTDataCon("Y", List(ASTTypeApply(ASTTypeVar("b"), List(ASTTypeVar("a")))))))
        evaluating { v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs) } should produce [KindConflictError]
    }

    it should "handle forall usage in data constructors" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", Nil, List(ASTDataCon("B", List(ASTForall(List("a"), ASTTypeVar("a"))))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        val fi = Type.lastForallId
        val fa = Forall(fi, List(Star), TGen(fi, 0)) fn TCon(ModuleId("Test", "A"), Star)
        defs.dcons should be === Map(ModuleId("Test", "B") -> fa)
    }

    it should "extend the tcons and dcons in the definitions list and leave all existing values unchanged" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", Nil, List(ASTDataCon("B", Nil)))
        val ModuleDefinitions(dcons, tcons, tcs, tcis, mts, mis) = v.addDataTypeDefs(Seq("Test" -> dtd), testDefs)
        dcons should be === testDefs.dcons + (ModuleId("Test", "A") -> TCon(ModuleId("Test", "A"), Star))
        tcons should be === testDefs.tcons + (ModuleId("Test", "B") -> TCon(ModuleId("Test", "A"), Star))
        tcs should be === testDefs.tcs
        tcis should be === testDefs.tcis
        mts should be === testDefs.mts
        (mis zip testDefs.mis) foreach { case ((k1, v1), (k2, v2)) =>
            k1 should be === k2
            v1 should equal(v2)
        }
    }

    // ------------------------------------------------------------------------

    behavior of "addTypeclassDefs"

    it should "throw an error for typeclasses with no type variables" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, Nil, Nil)
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        } should produce [VerifierMiscError]
    }

    it should "throw an error if the name of typeclass conflicts with an imported definition" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty.addClass("A", ModuleId("Prelude", "A"))))
        val tc = ASTClass("A", Nil, List("a"), Nil)
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        } should produce [NamespaceError]
    }

    it should "throw an error if a typeclass is defined more than once in a module" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, List("a"), Nil)
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc, "Test" -> tc), nullDefs)
        } should produce [ModuleDuplicateDefinition]
    }

    it should "produce typeclass definitions with single type parameters" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, List("a"), Nil)
        val defs = v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        defs.tcs should be === Map(
            ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Star)), Set.empty, Set.empty)
        )
    }

    it should "produce typeclass definitions with multiple type parameters" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, List("a", "b", "c"), Nil)
        val defs = v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        defs.tcs should be === Map(
            ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Star), TVar("b", Star), TVar("c", Star)), Set.empty, Set.empty)
        )
    }

    it should "throw an error for superclass references to classes that are not in scope" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("B", List(ASTClassRef("X", List("a"))), List("a"), Nil)
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        } should produce [UnknownTypeclassError]
    }

    it should "throw an error for typeclasses that pass undeclared type variables to a superclass" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))
            .addClass("B", ModuleId("Test", "B"))))
        val tcA = ASTClass("A", Nil, List("p"), Nil)
        val tcB = ASTClass("B", List(ASTClassRef("A", List("z"))), List("q"), Nil)
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tcA, "Test" -> tcB), nullDefs)
        } should produce [UnknownTypeVariableError]
    }

    it should "throw an error for superclass references with the wrong arity" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))
            .addClass("B", ModuleId("Test", "B"))))
        val tcA = ASTClass("A", Nil, List("p"), Nil)
        val tcB = ASTClass("B", List(ASTClassRef("A", List("x", "y"))), List("x", "y"), Nil)
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tcA, "Test" -> tcB), nullDefs)
        } should produce [TypeclassArityError]
    }

    it should "throw an error for recursive typeclass hierarchies" in {

        When("a typeclass has itself as a superclass")
        val v1 = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))))
        val tc = ASTClass("A", List(ASTClassRef("A", List("a"))), List("a"), Nil)
        evaluating {
            v1.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        } should produce [TypeclassRecursiveHeirarchyError]

        When("a typeclass's superclass has the typeclass as a superclass")
        val v2 = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))
            .addClass("B", ModuleId("Test", "B"))))
        val tcA = ASTClass("A", List(ASTClassRef("B", List("a"))), List("a"), Nil)
        val tcB = ASTClass("B", List(ASTClassRef("A", List("b"))), List("b"), Nil)
        evaluating {
            v2.addTypeclassDefs(Seq("Test" -> tcA, "Test" -> tcB), nullDefs)
        } should produce [TypeclassRecursiveHeirarchyError]
    }

    it should "produce typeclass definitions with a superclass" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))
            .addClass("B", ModuleId("Test", "B"))))
        val tcA = ASTClass("A", Nil, List("a"), Nil)
        val tcB = ASTClass("B", List(ASTClassRef("A", List("b"))), List("b"), Nil)
        val defs = v.addTypeclassDefs(Seq("Test" -> tcA, "Test" -> tcB), nullDefs)
        defs.tcs should be === Map(
            ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Star)), Set.empty, Set.empty),
            ModuleId("Test", "B") -> TypeclassDef(ModuleId("Test", "B"), List(IsIn(ModuleId("Test", "A"), List(TVar("b", Star)))), List(TVar("b", Star)), Set.empty, Set.empty)
        )
    }

    it should "produce typeclass definitions with multiple superclasses" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))
            .addClass("B", ModuleId("Test", "B"))
            .addClass("C", ModuleId("Test", "C"))))
        val tcA = ASTClass("A", Nil, List("a"), Nil)
        val tcB = ASTClass("B", Nil, List("b"), Nil)
        val tcC = ASTClass("C", List(ASTClassRef("A", List("c")), ASTClassRef("B", List("c"))), List("c"), Nil)
        val defs = v.addTypeclassDefs(Seq("Test" -> tcA, "Test" -> tcB, "Test" -> tcC), nullDefs)
        defs.tcs should be === Map(
            ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Star)), Set.empty, Set.empty),
            ModuleId("Test", "B") -> TypeclassDef(ModuleId("Test", "B"), Nil, List(TVar("b", Star)), Set.empty, Set.empty),
            ModuleId("Test", "C") -> TypeclassDef(ModuleId("Test", "C"), List(IsIn(ModuleId("Test", "A"), List(TVar("c", Star))), IsIn(ModuleId("Test", "B"), List(TVar("c", Star)))), List(TVar("c", Star)), Set.empty, Set.empty)
        )
    }

    it should "produce typeclass definitions with multiple superclasses over multiple type variables" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))
            .addClass("B", ModuleId("Test", "B"))
            .addClass("C", ModuleId("Test", "C"))))
        val tcA = ASTClass("A", Nil, List("a"), Nil)
        val tcB = ASTClass("B", Nil, List("b"), Nil)
        val tcC = ASTClass("C", List(ASTClassRef("A", List("y")), ASTClassRef("B", List("x"))), List("x", "y"), Nil)
        val defs = v.addTypeclassDefs(Seq("Test" -> tcA, "Test" -> tcB, "Test" -> tcC), nullDefs)
        defs.tcs should be === Map(
            ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Star)), Set.empty, Set.empty),
            ModuleId("Test", "B") -> TypeclassDef(ModuleId("Test", "B"), Nil, List(TVar("b", Star)), Set.empty, Set.empty),
            ModuleId("Test", "C") -> TypeclassDef(ModuleId("Test", "C"), List(IsIn(ModuleId("Test", "A"), List(TVar("y", Star))), IsIn(ModuleId("Test", "B"), List(TVar("x", Star)))), List(TVar("x", Star), TVar("y", Star)), Set.empty, Set.empty)
        )
    }

    it should "infer the kind of type variables using superclass information" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))
            .addClass("B", ModuleId("Test", "B"))))
        val defs0 = nullDefs.copy(tcs = nullDefs.tcs + (ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Kfun(Star, Star))), Set.empty, Set.empty)))
        val tc = ASTClass("B", List(ASTClassRef("A", List("b"))), List("b"), Nil)
        val defs = v.addTypeclassDefs(Seq("Test" -> tc), defs0)
        defs.tcs should be === defs0.tcs +
            (ModuleId("Test", "B") -> TypeclassDef(ModuleId("Test", "B"), List(IsIn(ModuleId("Test", "A"), List(TVar("b", Kfun(Star, Star))))), List(TVar("b", Kfun(Star, Star))), Set.empty, Set.empty))
    }

    it should "throw an error for typeclass members that don't use the class type variables" in {
        val v = new ModuleVerifier(nullScopes)

        When("the typeclass has one parameter")
        val tc1 = ASTClass("A", Nil, List("a"), List(ASTClassMemberDef("do-a", ASTQType(Nil, ASTTypeVar("b")))))
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc1), nullDefs)
        } should produce [TypeclassIllegalMemberDefinition]

        When("the typeclass has multiple parameters")
        val tc2 = ASTClass("A", Nil, List("a", "b"), List(ASTClassMemberDef("do-a", ASTQType(Nil, ASTTypeVar("a")))))
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc2), nullDefs)
        } should produce [TypeclassIllegalMemberDefinition]
    }

    it should "throw an error for duplicate typeclass members" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, List("a"), List(ASTClassMemberDef("do-a", ASTQType(Nil, ASTTypeVar("a"))), ASTClassMemberDef("do-a", ASTQType(Nil, ASTTypeVar("a")))))
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        } should produce [TypeclassDuplicateMemberDefinitionError]
    }

    it should "throw an error for duplicate typeclass member implementations" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, List("a"), List(
            ASTClassMemberDef("do-a", ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a"))))),
            ASTClassMemberImpl("do-a", ASTFunction(List("a"), ASTValueRead("a"))),
            ASTClassMemberImpl("do-a", ASTFunction(List("a"), ASTValueRead("a")))))
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        } should produce [TypeclassDuplicateMemberImplementationError]
    }

    it should "throw an error for typeclass members implementations that have no corresponding definition"  in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, List("a"), List(ASTClassMemberImpl("do-a", ASTFunction(List("a"), ASTValueRead("a")))))
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        } should produce [TypeclassImplementsUnknownMemberError]
    }

    it should "infer the kind of type variables using member definitions" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, List("a", "b"), List(ASTClassMemberDef("do-a", ASTQType(Nil, ASTTypeApply(ASTTypeVar("a"), List(ASTTypeVar("b")))))))
        val defs = v.addTypeclassDefs(Seq("Test" -> tc), nullDefs)
        defs.tcs should be === Map(
            ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Kfun(Star, Star)), TVar("b", Star)), Set("do-a"), Set.empty)
        )
    }

    it should "throw an error if the kind of type variables inferred from member definitions conflicts with the kind inferred from superclass information" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addClass("A", ModuleId("Test", "A"))
            .addClass("B", ModuleId("Test", "B"))))
        val defs0 = nullDefs.copy(tcs = nullDefs.tcs + (ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Kfun(Star, Star))), Set.empty, Set.empty)))
        val tc = ASTClass("B", List(ASTClassRef("A", List("b"))), List("b"), List(ASTClassMemberDef("do-b", ASTQType(Nil, ASTTypeApply(ASTTypeVar("b"), List(ASTTypeVar("b")))))))
        evaluating {
            v.addTypeclassDefs(Seq("Test" -> tc), defs0)
        } should produce [KindConflictError]
    }

    it should "extend the tcs in the definitions list and leave all existing values unchanged" in {
        val v = new ModuleVerifier(nullScopes)
        val tc = ASTClass("A", Nil, List("a"), Nil)
        val ModuleDefinitions(dcons, tcons, tcs, tcis, mts, mis) = v.addTypeclassDefs(Seq("Test" -> tc), testDefs)
        dcons should be === testDefs.dcons
        tcons should be === testDefs.tcons
        tcs should be === testDefs.tcs + (ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Star)), Set.empty, Set.empty))
        tcis should be === testDefs.tcis
        mts should be === testDefs.mts
        (mis zip testDefs.mis) foreach { case ((k1, v1), (k2, v2)) =>
            k1 should be === k2
            v1 should equal(v2)
        }
    }

    // ------------------------------------------------------------------------

    behavior of "addTypeclassInstances"
    ignore should "throw an error if the typeclass is not in scope" in {}
    ignore should "throw an error if the typeclass is provided the wrong number of types" in {}
    ignore should "throw an error if the typeclass is provided types of the wrong kinds" in {}
    ignore should "throw an error if the typeclass is provided non-concrete types" in {}
    ignore should "throw an error if the instance provides a duplicate implementation for a member" in {}
    ignore should "throw an error if the instance implements members that were not defined in the typeclass" in {}
    ignore should "throw an error if the instance does not implement all the members defined in the typeclass" in {}
    ignore should "produce typeclass instances" in {}
    ignore should "produce typeclass instances with members that make use of member-specific predicates" in {}
    ignore should "extend the tcis in the definitions list and leave all existing values unchanged" in {}

    // ------------------------------------------------------------------------

    behavior of "addMemberDefs"

    it should "throw an error if the name of a member conflicts with an imported definition" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty.addMember("fn", ModuleId("Prelude", "fn"))))
        evaluating {
            v.addMemberDefs(Seq(
                "Test" -> ASTDef("fn", ASTQType(Nil, ASTTypeCon("Unit")))
            ), nullDefs)
        } should produce [NamespaceError]
    }

    it should "throw an error if a member is defined more than once" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty.addTCon("Unit", ModuleId("Prelude", "Unit"))))
        val defs = ModuleDefinitions(
            Map(ModuleId("Prelude", "Unit") -> TCon(ModuleId("Prelude", "Unit"), Star)),
            Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
        evaluating {
            v.addMemberDefs(Seq(
                "Test" -> ASTDef("A", ASTQType(Nil, ASTTypeCon("Unit"))),
                "Test" -> ASTDef("A", ASTQType(Nil, ASTTypeCon("Unit")))), defs)
        } should produce [ModuleDuplicateDefinition]
    }

    ignore should "throw an error if a member name overlaps with a typeclass member name" in {}
    ignore should "extend the mts in the definitions list and leave all existing values unchanged" in {}

    // ------------------------------------------------------------------------

    behavior of "addTypeclassMemberDefs"
    ignore should "handle members with additional typeclass predicates" in {}
    ignore should "throw an error if a member has additional typeclass predicates that cause a kind mismatch" in {}
    ignore should "extend the mts in the definitions list and leave all existing values unchanged" in {}

    // ------------------------------------------------------------------------

    behavior of "addMemberImplementations"
    ignore should "throw an error if there are implementations missing for defined members" in {}
    ignore should "throw an error if multiple implemenations are provided for the same id"
    ignore should "throw an error if there is a cycle in initialising a group of members"
    ignore should "extend the mis in the definitions list and leave all existing values unchanged" in {}

    // ------------------------------------------------------------------------

    behavior of "getMemberType"
    ignore should "construct a qualified type from the AST for a type" in {}

    // ------------------------------------------------------------------------

    behavior of "getPredicates"
    ignore should "throw an error if a referenced type variable is out of scope" in {}
    ignore should "throw an error if there is a kind mismatch between type and typeclass reference" in {}
    ignore should "construct a list of predicates from a list of AST typeclass references" in {}

    // ------------------------------------------------------------------------

    behavior of "lookupInstanceType"
    ignore should "throw an error if the type has too many parameters applied" in {}
    ignore should "throw an error if the type has no parameters applied" in {}
    ignore should "construct a type for a typeclass instance parameter" in {}
}
