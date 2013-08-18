package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.ast._
import tap.{LocalId, ModuleId, InstId}
import tap.verifier.defs.{DefinitionsLookup, ModuleDefinitions}
import tap.verifier.ModuleVerifier
import tap.verifier.errors._
import tap.types._
import tap.types.kinds._
import tap.types.classes.{Qual, IsIn, TypeclassDef}
import tap.types.classes.ClassEnvironments.Inst
import tap.ir._
import language.reflectiveCalls
import test.TapNodeEquality
import tap.util.trace

class ModuleVerifierTests extends FlatSpec with TapNodeEquality with GivenWhenThen {

    val nullDefs = ModuleDefinitions.defaults
    val nullScopes = Map("Test" -> DefinitionsLookup.empty)

    val testDefs = nullDefs.copy(
        tcons = nullDefs.tcons ++ Map(ModuleId("Test", "X") -> TCon(ModuleId("Test", "X"), Star),
            ModuleId("Test", "X1") -> TCon(ModuleId("Test", "X1"), Kfun(Star, Star))),
        dcons = nullDefs.dcons ++ Map(ModuleId("Test", "X") -> TCon(ModuleId("Test", "X"), Star)),
        tcs = nullDefs.tcs ++ Map(ModuleId("Test", "Y") -> TypeclassDef(ModuleId("Test", "Y"), Nil, List(TVar("a", Star)), Set("yfn"), Set.empty),
            ModuleId("Test", "Y2") -> TypeclassDef(ModuleId("Test", "Y2"), Nil, List(TVar("a", Star), TVar("b", Star)), Set("yfn"), Set.empty)),
        tcis = nullDefs.tcis ++ Map(ModuleId("Test", "Y") -> List(Inst("Test", Nil, IsIn(ModuleId("Test", "Y"), List(Type.tString))))),
        mts = nullDefs.mts ++ Map(ModuleId("Test", "z") -> Qual(Nil, TCon(ModuleId("Test", "X"), Star)),
            ModuleId("Test", "yfn") -> Qual(List(IsIn(ModuleId("Test", "Y"), List(TVar("a", Star)))), TVar("a", Star) fn TVar("a", Star))),
        mis = nullDefs.mis ++ Map(ModuleId("Test", "z") -> ValueReadExpr(ModuleId("Test", "X")))
    )

    val testScopes = Map("Test" -> DefinitionsLookup.empty
            .addTCon("X", ModuleId("Test", "X"))
            .addTCon("X1", ModuleId("Test", "X1"))
            .addDCon("X", ModuleId("Test", "X"))
            .addClass("Y", ModuleId("Test", "Y"))
            .addClass("Y2", ModuleId("Test", "Y2"))
            .addMember("z", ModuleId("Test", "z")))

    // ------------------------------------------------------------------------

    behavior of "apply"

    it should "extend the verifiedDefs with the definitions found in all the passed modules" in {

        val mA = ASTModule("ModuleA", List(
            ASTDataType("TypeA", List("a"), List(
                ASTDataCon("DataA2", List(ASTTypeVar("a"))),
                ASTDataCon("DataA1", Nil))),
            ASTClass("ClassA", Nil, List("a"), List(
                ASTClassMemberDef("cmemberA", ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a"))))))),
            ASTClassInst("ClassA", List(ASTClassRef("ClassA", List("a"))), List(ASTTypeApply(ASTTypeCon("TypeA"), List(ASTTypeVar("a")))), List(
                ASTClassMemberImpl("cmemberA", ASTFunction(List("x"), ASTValueRead("x"))))),
            ASTLet("memberA", ASTFunction(List("x"), ASTValueRead("x")))))

        val mB = ASTModule("ModuleB", List(
            ASTDataType("TypeB", Nil, List(
                ASTDataCon("DataB", Nil))),
            ASTClass("ClassB", Nil, List("a"), List(
                ASTClassMemberDef("cmemberB", ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a"))))))),
            ASTClassInst("ClassB", Nil, List(ASTTypeCon("TypeB")), List(
                ASTClassMemberImpl("cmemberB", ASTFunction(List("x"), ASTValueRead("x"))))),
            ASTLet("memberB", ASTFunction(List("x"), ASTValueRead("x")))))

        val v = new ModuleVerifier(Map(
            "Test" -> testScopes("Test"),
            "ModuleA" -> DefinitionsLookup.empty
                    .addTCon("TypeA", ModuleId("ModuleA", "TypeA"))
                    .addDCon("DataA1", ModuleId("ModuleA", "DataA1"))
                    .addDCon("DataA2", ModuleId("ModuleA", "DataA2"))
                    .addClass("ClassA", ModuleId("ModuleA", "ClassA"))
                    .addMember("cmemberA", ModuleId("ModuleA", "cmemberA"))
                    .addMember("memberA", ModuleId("ModuleA", "memberA")),
            "ModuleB" -> DefinitionsLookup.empty
                    .addTCon("TypeB", ModuleId("ModuleB", "TypeB"))
                    .addDCon("DataB", ModuleId("ModuleB", "DataB"))
                    .addClass("ClassB", ModuleId("ModuleB", "ClassB"))
                    .addMember("cmemberB", ModuleId("ModuleB", "cmemberB"))
                    .addMember("memberB", ModuleId("ModuleB", "memberB"))
        ))

        val defs = v.apply(Seq(mA, mB), testDefs)

        val cmemberBFI = Type.lastForallId
        val cmemberAFI = cmemberBFI - 1
        val dataA1FI = cmemberAFI - 1
        val dataA2FI = dataA1FI - 1

        defs.tcons should be === testDefs.tcons +
                (ModuleId("ModuleA", "TypeA") -> TCon(ModuleId("ModuleA", "TypeA"), Kfun(Star, Star))) +
                (ModuleId("ModuleB", "TypeB") -> TCon(ModuleId("ModuleB", "TypeB"), Star))
        defs.dcons should be === testDefs.dcons +
                (ModuleId("ModuleA", "DataA2") -> Forall(dataA2FI, List(Star), TGen(dataA2FI, 0) fn TAp(TCon(ModuleId("ModuleA", "TypeA"), Kfun(Star, Star)), TGen(dataA2FI, 0)))) +
                (ModuleId("ModuleA", "DataA1") -> Forall(dataA1FI, List(Star), TAp(TCon(ModuleId("ModuleA", "TypeA"), Kfun(Star, Star)), TGen(dataA1FI, 0)))) +
                (ModuleId("ModuleB", "DataB") -> TCon(ModuleId("ModuleB", "TypeB"), Star))
        defs.tcs should be === testDefs.tcs +
                (ModuleId("ModuleA", "ClassA") -> TypeclassDef(ModuleId("ModuleA", "ClassA"), List(), List(TVar("a", Star)), Set("cmemberA"), Set())) +
                (ModuleId("ModuleB", "ClassB") -> TypeclassDef(ModuleId("ModuleB", "ClassB"), List(), List(TVar("a", Star)), Set("cmemberB"), Set()))
        defs.tcis should be === testDefs.tcis +
                (ModuleId("ModuleA", "ClassA") -> List(Inst("ModuleA", List(IsIn(ModuleId("ModuleA", "ClassA"), List(TVar("a", Star)))), IsIn(ModuleId("ModuleA", "ClassA"), List(TAp(TCon(ModuleId("ModuleA", "TypeA"), Kfun(Star, Star)), TVar("a", Star))))))) +
                (ModuleId("ModuleB", "ClassB") -> List(Inst("ModuleB", List(), IsIn(ModuleId("ModuleB", "ClassB"), List(TCon(ModuleId("ModuleB", "TypeB"), Star))))))
        defs.mts should be === testDefs.mts +
                (ModuleId("ModuleA", "cmemberA") -> Qual(List(IsIn(ModuleId("ModuleA", "ClassA"), List(TGen(cmemberAFI, 0)))), Forall(cmemberAFI, List(Star), TGen(cmemberAFI, 0) fn TGen(cmemberAFI, 0)))) +
                (ModuleId("ModuleB", "cmemberB") -> Qual(List(IsIn(ModuleId("ModuleB", "ClassB"), List(TGen(cmemberBFI, 0)))), Forall(cmemberBFI, List(Star), TGen(cmemberBFI, 0) fn TGen(cmemberBFI, 0))))

        val testMis = testDefs.mis +
                (ModuleId("ModuleA", "memberA") -> FunctionExpr(Argument("x"), ValueReadExpr(LocalId("x")))) +
                (InstId("ModuleA", ModuleId("ModuleA", "ClassA"), List(ModuleId("ModuleA", "TypeA")), "cmemberA") -> FunctionExpr(Argument("x"), ValueReadExpr(LocalId("x")))) +
                (ModuleId("ModuleB", "memberB") -> FunctionExpr(Argument("x"), ValueReadExpr(LocalId("x")))) +
                (InstId("ModuleB", ModuleId("ModuleB", "ClassB"), List(ModuleId("ModuleB", "TypeB")), "cmemberB") -> FunctionExpr(Argument("x"), ValueReadExpr(LocalId("x"))))

        defs.mis.size should be === testMis.size
        (defs.mis zip testMis) foreach { case ((k1, v1), (k2, v2)) =>
            k1 should be === k2
            v1 should equal(v2)
        }
    }

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
        defs.tcons should be === nullDefs.tcons + (ModuleId("Test", "A") -> TCon(ModuleId("Test", "A"), Star))
    }

    it should "handle type constructors with type variables" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", List("p", "q"), Nil)
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        defs.tcons should be === nullDefs.tcons + (ModuleId("Test", "A") -> TCon(ModuleId("Test", "A"), Kfun(Star, Kfun(Star, Star))))
    }

    it should "handle data constructors with no arguments" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", Nil, List(ASTDataCon("B", Nil)))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        defs.dcons should be === nullDefs.dcons + (ModuleId("Test", "B") -> TCon(ModuleId("Test", "A"), Star))
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
        defs.dcons should be === nullDefs.dcons + (ModuleId("Test", "B") -> (tA fn tB))
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
        defs.dcons should be === nullDefs.dcons +
                (ModuleId("Test", "A") -> (tB fn tA)) +
                (ModuleId("Test", "B") -> (tA fn tB))
    }

    it should "handle self-referential data constructors" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
            .addTCon("A", ModuleId("Test", "A"))))
        val dtd = ASTDataType("A", Nil, List(ASTDataCon("B", List(ASTTypeCon("A")))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        val tA = TCon(ModuleId("Test", "A"), Star)
        defs.dcons should be === nullDefs.dcons + (ModuleId("Test", "B") -> (tA fn tA))
    }

    it should "handle data constructors with quantified type variable arguments" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", List("p", "q"), List(ASTDataCon("B", List(ASTTypeVar("p"), ASTTypeVar("q")))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        val fi = Type.lastForallId
        val fa = Forall(fi, List(Star, Star), TGen(fi, 0) fn (TGen(fi, 1) fn TAp(TAp(TCon(ModuleId("Test", "A"), Kfun(Star, Kfun(Star, Star))), TGen(fi, 0)), TGen(fi, 1))))
        defs.dcons should be === nullDefs.dcons + (ModuleId("Test", "B") -> fa)
    }

    it should "infer the kind of type variables based upon their usage in data constructors" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", List("p", "q"), List(ASTDataCon("B", List(ASTTypeApply(ASTTypeVar("p"), List(ASTTypeVar("q")))))))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), nullDefs)
        val fi = Type.lastForallId
        val fa = Forall(fi, List(Kfun(Star, Star), Star), TAp(TGen(fi, 0), TGen(fi, 1)) fn TAp(TAp(TCon(ModuleId("Test", "A"), Kfun(Kfun(Star, Star), Kfun(Star, Star))), TGen(fi, 0)), TGen(fi, 1)))
        defs.dcons should be === nullDefs.dcons + (ModuleId("Test", "B") -> fa)
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
        defs.dcons should be === nullDefs.dcons + (ModuleId("Test", "B") -> fa)
    }

    it should "extend the tcons and dcons in the definitions list and leave all existing values unchanged" in {
        val v = new ModuleVerifier(nullScopes)
        val dtd = ASTDataType("A", Nil, List(ASTDataCon("B", Nil)))
        val defs = v.addDataTypeDefs(Seq("Test" -> dtd), testDefs)
        defs.tcons should be === testDefs.tcons + (ModuleId("Test", "A") -> TCon(ModuleId("Test", "A"), Star))
        defs.dcons should be === testDefs.dcons + (ModuleId("Test", "B") -> TCon(ModuleId("Test", "A"), Star))
        defs.tcs should be === testDefs.tcs
        defs.tcis should be === testDefs.tcis
        defs.mts should be === testDefs.mts
        defs.mis.size should be === testDefs.mis.size
        (defs.mis zip testDefs.mis) foreach { case ((k1, v1), (k2, v2)) =>
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
        val defs = v.addTypeclassDefs(Seq("Test" -> tc), testDefs)
        defs.tcons should be === testDefs.tcons
        defs.dcons should be === testDefs.dcons
        defs.tcs should be === testDefs.tcs + (ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(TVar("a", Star)), Set.empty, Set.empty))
        defs.tcis should be === testDefs.tcis
        defs.mts should be === testDefs.mts
        defs.mis.size should be === testDefs.mis.size
        (defs.mis zip testDefs.mis) foreach { case ((k1, v1), (k2, v2)) =>
            k1 should be === k2
            v1 should equal(v2)
        }
    }

    // ------------------------------------------------------------------------

    behavior of "addTypeclassInstances"

    it should "throw an error if the typeclass is not in scope" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.addTypeclassInstances(Seq(
                "Test" -> ASTClassInst("Foof", Nil, List(ASTTypeCon("X")), Nil)
            ), testDefs)
        } should produce [UnknownTypeclassError]
    }

    it should "throw an error if the typeclass is provided the wrong number of types" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.addTypeclassInstances(Seq(
                "Test" -> ASTClassInst("Y", Nil, List(ASTTypeCon("X"), ASTTypeCon("X")), Nil)
            ), testDefs)
        } should produce [TypeclassArityError]
    }

    it should "throw an error if the typeclass is provided types of the wrong kinds" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.addTypeclassInstances(Seq(
                "Test" -> ASTClassInst("Y", Nil, List(ASTTypeCon("X1")), Nil)
            ), testDefs)
        } should produce [TypeclassIllegalParameterError]
    }

    it should "throw an error if the typeclass is provided non-concrete types" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.addTypeclassInstances(Seq(
                "Test" -> ASTClassInst("Y", Nil, List(ASTTypeVar("a")), Nil)
            ), testDefs)
        } should produce [TypeclassIllegalParameterError]
    }

    it should "throw an error if the instance provides a duplicate implementation for a member" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.addTypeclassInstances(Seq(
                "Test" -> ASTClassInst("Y", Nil, List(ASTTypeCon("X")), List(
                    ASTClassMemberImpl("yfn", ASTFunction(List("x"), ASTValueRead("x"))),
                    ASTClassMemberImpl("yfn", ASTFunction(List("x"), ASTValueRead("x")))
                ))
            ), testDefs)
        } should produce [InstanceDuplicateMemberError]
    }

    it should "throw an error if the instance implements members that were not defined in the typeclass" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.addTypeclassInstances(Seq(
                "Test" -> ASTClassInst("Y", Nil, List(ASTTypeCon("X")), List(
                    ASTClassMemberImpl("yfn", ASTFunction(List("x"), ASTValueRead("x"))),
                    ASTClassMemberImpl("hwaet", ASTFunction(List("x"), ASTValueRead("x")))
                ))
            ), testDefs)
        } should produce [InstanceUnknownMemberError]
    }

    it should "throw an error if the instance does not implement all the members defined in the typeclass" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.addTypeclassInstances(Seq(
                "Test" -> ASTClassInst("Y", Nil, List(ASTTypeCon("X")), Nil)
            ), testDefs)
        } should produce [InstanceIncompleteError]
    }

    it should "produce typeclass instances" in {
        val v = new ModuleVerifier(testScopes)
        val defs = v.addTypeclassInstances(Seq(
            "Test" -> ASTClassInst("Y", Nil, List(ASTTypeCon("X")), List(
                ASTClassMemberImpl("yfn", ASTFunction(List("x"), ASTValueRead("x")))
            ))
        ), testDefs)
        val origTCIs = testDefs.tcis(ModuleId("Test", "Y"))
        defs.tcis should be === testDefs.tcis + (ModuleId("Test", "Y") ->
                (Inst("Test", Nil, IsIn(ModuleId("Test", "Y"), List(testDefs.tcons(ModuleId("Test", "X"))))) :: origTCIs))
    }

    it should "produce typeclass instances defined with extended context" in {
        val v = new ModuleVerifier(testScopes)
        val defs = v.addTypeclassInstances(Seq(
            "Test" -> ASTClassInst("Y", List(ASTClassRef("Y", List("a"))), List(ASTTypeApply(ASTTypeCon("X1"), List(ASTTypeVar("a")))), List(
                ASTClassMemberImpl("yfn", ASTFunction(List("x"), ASTValueRead("x")))
            ))
        ), testDefs)
        val origTCIs = testDefs.tcis(ModuleId("Test", "Y"))
        defs.tcis should be === testDefs.tcis + (ModuleId("Test", "Y") ->
                (Inst("Test",
                    List(IsIn(ModuleId("Test", "Y"), List(TVar("a", Star)))),
                    IsIn(ModuleId("Test", "Y"), List(TAp(testDefs.tcons(ModuleId("Test", "X1")), TVar("a", Star)))))
                        :: origTCIs))
    }

    it should "extend the tcis in the definitions list and leave all existing values unchanged" in {
        val v = new ModuleVerifier(testScopes)
        val defs = v.addTypeclassInstances(Seq(
            "Test" -> ASTClassInst("Y", List(ASTClassRef("Y", List("a"))), List(ASTTypeApply(ASTTypeCon("X1"), List(ASTTypeVar("a")))), List(
                ASTClassMemberImpl("yfn", ASTFunction(List("x"), ASTValueRead("x")))
            ))
        ), testDefs)
        defs.tcons should be === testDefs.tcons
        defs.dcons should be === testDefs.dcons
        defs.tcs should be === testDefs.tcs
        val origTCIs = testDefs.tcis(ModuleId("Test", "Y"))
        defs.tcis should be === testDefs.tcis + (ModuleId("Test", "Y") ->
                (Inst("Test",
                    List(IsIn(ModuleId("Test", "Y"), List(TVar("a", Star)))),
                    IsIn(ModuleId("Test", "Y"), List(TAp(testDefs.tcons(ModuleId("Test", "X1")), TVar("a", Star)))))
                        :: origTCIs))
        defs.mts should be === testDefs.mts
        defs.mis.size should be === testDefs.mis.size
        (defs.mis zip testDefs.mis) foreach { case ((k1, v1), (k2, v2)) =>
            k1 should be === k2
            v1 should equal(v2)
        }
    }

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
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.defaults))
        val defs = ModuleDefinitions.defaults
        evaluating {
            v.addMemberDefs(Seq(
                "Test" -> ASTDef("A", ASTQType(Nil, ASTTypeCon("Unit"))),
                "Test" -> ASTDef("A", ASTQType(Nil, ASTTypeCon("Unit")))), defs)
        } should produce [ModuleDuplicateDefinition]
    }

    it should "extend the mts in the definitions list and leave all existing values unchanged" in {
        val v = new ModuleVerifier(testScopes)
        val defs = v.addMemberDefs(Seq("Test" -> ASTDef("A", ASTQType(Nil, ASTTypeCon("X")))), testDefs)
        defs.tcons should be === testDefs.tcons
        defs.dcons should be === testDefs.dcons
        defs.tcs should be === testDefs.tcs
        defs.tcis should be === testDefs.tcis
        defs.mts should be === testDefs.mts + (ModuleId("Test", "A") -> Qual(Nil, testDefs.tcons(ModuleId("Test", "X"))))
        defs.mis.size should be === testDefs.mis.size
        (defs.mis zip testDefs.mis) foreach { case ((k1, v1), (k2, v2)) =>
            k1 should be === k2
            v1 should equal(v2)
        }
    }

    // ------------------------------------------------------------------------

    behavior of "addTypeclassMemberDefs"

    it should "throw an error if the name of a member conflicts with an imported definition" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
                .addClass("C", ModuleId("Test", "C"))
                .addMember("z", ModuleId("Prelude", "z"))))
        val c = ASTClass("C", Nil, List("a"), List(ASTClassMemberDef("z", ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))))))
        val defs = v.addTypeclassDefs(Seq("Test" -> c), nullDefs)
        evaluating {
            v.addTypeclassMemberDefs(Seq("Test" -> c), defs)
        } should produce [NamespaceError]
    }

    it should "throw an error if a member name overlaps with a module member name" in {
        val v = new ModuleVerifier(Map("Test" -> DefinitionsLookup.empty
                .addClass("C", ModuleId("Test", "C"))
                .addMember("z", ModuleId("Test", "z"))))
        val c = ASTClass("C", Nil, List("a"), List(ASTClassMemberDef("z", ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))))))
        val defs0 = v.addMemberDefs(Seq("Test" -> ASTDef("z", ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))))), nullDefs)
        val defs1 = v.addTypeclassDefs(Seq("Test" -> c), defs0)
        evaluating {
            v.addTypeclassMemberDefs(Seq("Test" -> c), defs1)
        } should produce [ModuleDuplicateDefinition]
    }

    it should "handle members with additional typeclass predicates" in {
        val v = new ModuleVerifier(Map("Test" -> testScopes("Test")
                .addClass("C", ModuleId("Test", "C"))))
        val c = ASTClass("C", Nil, List("a"), List(ASTClassMemberDef("ccc",
            ASTQType(List(ASTClassRef("Y", List("b"))), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("b"), ASTTypeVar("a")))))))
        val defs0 = v.addTypeclassDefs(Seq("Test" -> c), testDefs)
        val defs1 = v.addTypeclassMemberDefs(Seq("Test" -> c), defs0)
        val fi = Type.lastForallId
        defs1.mts(ModuleId("Test", "ccc")) should be === Qual(List(
            IsIn(ModuleId("Test", "C"), List(TGen(fi, 0))),
            IsIn(ModuleId("Test", "Y"), List(TGen(fi, 1)))),
                Forall(fi, List(Star, Star), TGen(fi, 0) fn (TGen(fi, 1) fn TGen(fi, 0))))
    }

    it should "throw an error if a member has additional typeclass predicates that cause a kind mismatch" in {
        val v = new ModuleVerifier(Map("Test" -> testScopes("Test")
                .addClass("C", ModuleId("Test", "C"))))
        val c = ASTClass("C", Nil, List("a", "b"), List(
            ASTClassMemberDef("bbb", ASTQType(Nil, ASTFunctionType(List(ASTTypeApply(ASTTypeVar("a"), List(ASTTypeVar("b"))), ASTTypeVar("b"))))),
            ASTClassMemberDef("ccc", ASTQType(List(ASTClassRef("Y2", List("a", "c"))), ASTFunctionType(List(ASTTypeApply(ASTTypeVar("a"), List(ASTTypeVar("b"))), ASTTypeVar("c"), ASTTypeVar("c")))))))

        val defs0 = v.addTypeclassDefs(Seq("Test" -> c), testDefs)
        evaluating { v.addTypeclassMemberDefs(Seq("Test" -> c), defs0) } should produce [KindMismatchError]
    }

    it should "extend the mts in the definitions list and leave all existing values unchanged" in {
        val v = new ModuleVerifier(Map("Test" -> testScopes("Test")
                .addClass("C", ModuleId("Test", "C"))))
        val c = ASTClass("C", Nil, List("a"), List(ASTClassMemberDef("ccc",
            ASTQType(Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))))))
        val defs0 = v.addTypeclassDefs(Seq("Test" -> c), testDefs)
        val defs1 = v.addTypeclassMemberDefs(Seq("Test" -> c), defs0)
        val fi = Type.lastForallId
        defs1.tcons should be === defs0.tcons
        defs1.dcons should be === defs0.dcons
        defs1.tcs should be === defs0.tcs
        defs1.tcis should be === defs0.tcis
        defs1.mts should be === defs0.mts + (ModuleId("Test", "ccc") -> Qual(List(IsIn(ModuleId("Test", "C"), List(TGen(fi, 0)))),
            Forall(fi, List(Star), TGen(fi, 0) fn TGen(fi, 0))))
        defs1.mis.size should be === defs0.mis.size
        (defs1.mis zip defs0.mis) foreach { case ((k1, v1), (k2, v2)) =>
            k1 should be === k2
            v1 should equal(v2)
        }
    }

    // ------------------------------------------------------------------------

    behavior of "addMemberImplementations"

    it should "throw an error if there are implementations missing for defined members" in {
        val v = new ModuleVerifier(testScopes)
        val mds = Seq("Test" -> new ASTDef("member", ASTQType(Nil, ASTTypeCon("X"))))
        val defs = v.addMemberDefs(mds, testDefs)
        evaluating {
            v.addMemberImplementations(Nil, mds, defs)
        } should produce [ModuleMissingImplementationError]
    }

    it should "throw an error if multiple implemenations are provided for the same id" in {
        val v = new ModuleVerifier(nullScopes)
        val mi = new ASTLet("member", ASTFunction(List("x"), ASTValueRead("x")))
        val m = new ASTModule("Test", List(mi, mi))
        evaluating {
            v.addMemberImplementations(Seq(m), Nil, nullDefs)
        } should produce [ModuleDuplicateDefinition]
    }

    it should "throw an error if a member refers to itself in initialisation" in {
        val v = new ModuleVerifier(Map("Test" -> testScopes("Test")
                .addMember("memberX", ModuleId("Test", "memberX"))))
        val mi = new ASTLet("memberX", ASTValueRead("memberX"))
        val m = new ASTModule("Test", List(mi))
        evaluating {
            v.addMemberImplementations(Seq(m), Nil, nullDefs)
        } should produce [ModuleMemberInitRecursiveError]
    }

    it should "throw an error if there is a cycle in initialising a group of members" in {
        val v = new ModuleVerifier(Map("Test" -> testScopes("Test")
                .addMember("memberX", ModuleId("Test", "memberX"))
                .addMember("memberY", ModuleId("Test", "memberY"))))
        val miX = new ASTLet("memberX", ASTValueRead("memberY"))
        val miY = new ASTLet("memberY", ASTValueRead("memberX"))
        val m = new ASTModule("Test", List(miX, miY))
        evaluating {
            v.addMemberImplementations(Seq(m), Nil, nullDefs)
        } should produce [ModuleMemberInitCycleError]
    }

    it should "extend the mis in the definitions list and leave all existing values unchanged" in {
        val v = new ModuleVerifier(testScopes)
        val mi = new ASTLet("member", ASTFunction(List("x"), ASTValueRead("x")))
        val m = new ASTModule("Test", List(mi))
        val defs = v.addMemberImplementations(Seq(m), Nil, testDefs)
        defs.tcons should be === testDefs.tcons
        defs.dcons should be === testDefs.dcons
        defs.tcs should be === testDefs.tcs
        defs.tcis should be === testDefs.tcis
        defs.mts should be === testDefs.mts
        val testMis = testDefs.mis + (ModuleId("Test", "member") -> FunctionExpr(Argument("x"), ValueReadExpr(LocalId("x"))))
        defs.mis.size should be === testMis.size
        (defs.mis zip testMis) foreach { case ((k1, v1), (k2, v2)) =>
            k1 should be === k2
            v1 should equal(v2)
        }
    }

    // ------------------------------------------------------------------------

    behavior of "getPredicates"

    it should "throw an error if a referenced type variable is out of scope" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.getPredicates(
                testScopes("Test").tcs,
                testDefs.tcs,
                List(ASTClassRef("Y", List("a"))),
                Map.empty)
        } should produce [UnknownTypeVariableError]
    }

    it should "throw an error if there is an arity mismatch with the referenced class" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.getPredicates(
                testScopes("Test").tcs,
                testDefs.tcs,
                List(ASTClassRef("Y", List("a", "b"))),
                Map("a" -> TVar("a", Star)))
        } should produce [TypeclassArityError]
    }

    it should "throw an error if there is a kind mismatch between type and typeclass reference" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.getPredicates(
                testScopes("Test").tcs,
                testDefs.tcs,
                List(ASTClassRef("Y", List("a"))),
                Map("a" -> TVar("a", Kfun(Star, Star))))
        } should produce [KindMismatchError]
    }

    it should "construct a list of predicates from a list of AST typeclass references" in {
        val v = new ModuleVerifier(testScopes)
        v.getPredicates(
            testScopes("Test").tcs,
            testDefs.tcs,
            List(ASTClassRef("Y", List("a")), ASTClassRef("Y", List("b"))),
            Map("a" -> TVar("a", Star), "b" -> TVar("b", Star))) should be ===
        List(IsIn(ModuleId("Test", "Y"), List(TVar("a", Star))),
            IsIn(ModuleId("Test", "Y"), List(TVar("b", Star))))
    }

    // ------------------------------------------------------------------------

    behavior of "getMemberType"

    it should "throw an error if any of the referenced types are not in scope" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.getMemberType(
                ModuleId("Test", "a"),
                ASTQType(Nil, ASTTypeCon("Nonexist")),
                testDefs)
        } should produce [UnknownTypeConstructorError]
    }

    it should "throw an error if any of the qualified type variables are not reachable in the type" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.getMemberType(
                ModuleId("Test", "a"),
                ASTQType(List(ASTClassRef("Y", List("a"))), ASTTypeCon("X")),
                testDefs)
        } should produce [UnknownTypeVariableError]
    }

    it should "construct a qualified type from the AST for a type" in {
        val v = new ModuleVerifier(testScopes)
        v.getMemberType(
            ModuleId("Test", "a"),
            ASTQType(Nil, ASTTypeCon("X")),
            testDefs) should be ===
        Qual(Nil, TCon(ModuleId("Test", "X"), Star))
    }

    it should "construct a qualified type from the AST for a type with predicates" in {
        val v = new ModuleVerifier(testScopes)
        val qt = v.getMemberType(
            ModuleId("Test", "a"),
            ASTQType(List(ASTClassRef("Y", List("a"))), ASTTypeApply(ASTTypeCon("X1"), List(ASTTypeVar("a")))),
            testDefs)
        val fi = Type.lastForallId
        qt should be === Qual(List(
            IsIn(ModuleId("Test", "Y"), List(TGen(fi, 0)))),
            Forall(fi, List(Star), TAp(TCon(ModuleId("Test", "X1"), Kfun(Star, Star)), TGen(fi, 0))))
    }

    it should "throw an error if type variables declared by forall have overlapping names" in {
        val v = new ModuleVerifier(testScopes)

        Given("a type where the name overlap occurs by shadowing")
        evaluating {
            v.getMemberType(
                ModuleId("Test", "a"),
                ASTQType(Nil, ASTFunctionType(List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))), ASTTypeVar("a")))),
                testDefs)
        } should produce [TypeVariableOverlapError]

        Given("a type where the name appears in parallel foralls")
        evaluating {
            v.getMemberType(
                ModuleId("Test", "a"),
                ASTQType(Nil, ASTFunctionType(List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))), ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))), ASTTypeVar("b")))),
                testDefs)
        } should produce [TypeVariableOverlapError]
    }

    it should "allow qualification of variables that appear in an inner forall" in {
        val v = new ModuleVerifier(testScopes)
        val qt = v.getMemberType(
            ModuleId("Test", "a"),
            ASTQType(List(ASTClassRef("Y", List("a"))), ASTFunctionType(List(ASTForall(List("a"), ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))), ASTTypeCon("X")))),
            testDefs)
        val fi = Type.lastForallId
        qt should be === Qual(List(IsIn(ModuleId("Test", "Y"), List(TGen(fi, 0)))),
            Forall(fi, List(Star), TGen(fi, 0) fn TGen(fi, 0)) fn TCon(ModuleId("Test", "X"), Star))
    }

    // ------------------------------------------------------------------------

    behavior of "lookupInstanceParamType"

    it should "throw an error if the type has parameters applied but does not accept parameters" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.lookupInstanceParamType(
                testScopes("Test").tcons,
                testDefs.tcons,
                ASTTypeApply(ASTTypeCon("X"), List(ASTTypeVar("a"))))
        } should produce [TypeConstructorNoArgsError]
    }

    it should "throw an error if the type has too many parameters applied" in {
        val v = new ModuleVerifier(testScopes)
        evaluating {
            v.lookupInstanceParamType(
                testScopes("Test").tcons,
                testDefs.tcons,
                ASTTypeApply(ASTTypeCon("X1"), List(ASTTypeVar("a"), ASTTypeVar("b"))))
        } should produce [TypeConstructorTooManyArgsError]
    }

    it should "construct a type for a typeclass instance parameter" in {
        val v = new ModuleVerifier(testScopes)
        v.lookupInstanceParamType(
            testScopes("Test").tcons,
            testDefs.tcons,
            ASTTypeApply(ASTTypeCon("X1"), List(ASTTypeVar("a")))) should be ===
        TAp(testDefs.tcons(ModuleId("Test", "X1")), TVar("a", Star))
    }
}
