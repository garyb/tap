package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._
import tap.ast._
import tap.{ast, ModuleId}
import tap.verifier.defs.{ImportedDefinitions, ModuleDefinitions}
import tap.verifier.ModuleVerifier
import tap.verifier.errors._
import tap.types._
import tap.types.kinds._
import tap.types.classes.{Qual, IsIn, TypeclassDef}
import tap.types.classes.ClassEnvironments.Inst
import tap.ir.ValueReadExpr
import tap.ast.ASTForall
import tap.ast.ASTDataTypeDefinition
import tap.verifier.errors.KindConflictError
import tap.ir.ValueReadExpr
import tap.types.TCon
import tap.types.Tyvar
import tap.verifier.errors.UnknownTypeVariableError
import tap.ast.ASTTypeVar
import tap.ast.ASTTypeCon
import tap.ast.ASTTypeClassReference
import tap.ast.ASTTypeClassDefinition
import tap.types.classes.ClassEnvironments.Inst
import tap.types.TGen
import tap.verifier.errors.NamespaceError
import tap.types.Tycon
import tap.types.Forall
import tap.types.TAp
import tap.ast.ASTTypeApply
import tap.types.classes.TypeclassDef
import tap.types.kinds.Kfun
import tap.ast._
import tap.util.GraphError

class ModuleVerifierTests extends FlatSpec with GivenWhenThen {

	val nullDefs = ModuleDefinitions(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
	val nullScopes = Map("Test" -> ImportedDefinitions.empty)
	val testDefs =
		ModuleDefinitions(
			Map(ModuleId("Test", "X") -> TCon(Tycon(ModuleId("Test", "X"), Star))),
			Map(ModuleId("Test", "X") -> TCon(Tycon(ModuleId("Test", "X"), Star))),
			Map(ModuleId("Test", "Y") -> TypeclassDef(ModuleId("Test", "Y"), Nil, Nil, Set.empty, Set.empty)),
			Map(ModuleId("Test", "Y") -> List(Inst("Test", Nil, IsIn(ModuleId("Test", "Y"), List(Type.tString))))),
			Map(ModuleId("Test", "z") -> Qual(Nil, TCon(Tycon(ModuleId("Test", "X"), Star)))),
			Map(ModuleId("Test", "z") -> ValueReadExpr(ModuleId("Test", "X"))))

	// ------------------------------------------------------------------------

	behavior of "apply"

	ignore should "throw an error if there are multiple data type definitions with the same name in a module" in {}
	ignore should "throw an error if there are multiple typeclasses with the same name in a module" in {}
	ignore should "throw an error if there are multiple member definitions with the same name in a module" in {}

	// ------------------------------------------------------------------------

	behavior of "addDataTypeDefs"

	ignore should "throw an error if the name of a type constructor conflicts with an imported definition" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions(Map("A" -> ModuleId("Prelude", "A")), Map.empty, Map.empty, Map.empty)))
		val dtd = ASTDataTypeDefinition("A", Nil, Nil)
		evaluating {
			v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		} should produce [NamespaceError]
	}

	ignore should "throw an error if the name of a data constructor conflicts with an imported definition" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions(Map.empty, Map("B" -> ModuleId("Prelude", "B")), Map.empty, Map.empty)))
		val dtd = ASTDataTypeDefinition("A", Nil, List(ASTDataTypeConstructor("B", Nil)))
		evaluating {
			v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		} should produce [NamespaceError]
	}

	it should "handle type constructors without type variables" in {
		val v = new ModuleVerifier(nullScopes)
		val dtd = ASTDataTypeDefinition("A", Nil, Nil)
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		defs.tcons should be === Map(ModuleId("Test", "A") -> TCon(Tycon(ModuleId("Test", "A"), Star)))
	}

	it should "handle type constructors with type variables" in {
		val v = new ModuleVerifier(nullScopes)
		val dtd = ASTDataTypeDefinition("A", List("p", "q"), Nil)
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		defs.tcons should be === Map(ModuleId("Test", "A") -> TCon(Tycon(ModuleId("Test", "A"), Kfun(Star, Kfun(Star, Star)))))
	}

	it should "handle data constructors with no arguments" in {
		val v = new ModuleVerifier(nullScopes)
		val dtd = ASTDataTypeDefinition("A", Nil, List(ASTDataTypeConstructor("B", Nil)))
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		defs.dcons should be === Map(ModuleId("Test", "B") -> TCon(Tycon(ModuleId("Test", "A"), Star)))
	}

	it should "handle data constructors with non-type variable arguments" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addTCon("A", ModuleId("Test", "A"))
			.addTCon("B", ModuleId("Test", "B"))))
		val dtd1 = ASTDataTypeDefinition("A", Nil, Nil)
		val dtd2 = ASTDataTypeDefinition("B", Nil, List(ASTDataTypeConstructor("B", List(ASTTypeCon("A")))))
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd1, ModuleId("Test", "B") -> dtd2), nullDefs)
		val tA = TCon(Tycon(ModuleId("Test", "A"), Star))
		val tB = TCon(Tycon(ModuleId("Test", "B"), Star))
		defs.dcons should be === Map(
			ModuleId("Test", "B") -> (tA fn tB)
		)
	}

	it should "handle circular dependencies between type constructors" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addTCon("A", ModuleId("Test", "A"))
			.addTCon("B", ModuleId("Test", "B"))))
		val dtd1 = ASTDataTypeDefinition("A", Nil, List(ASTDataTypeConstructor("A", List(ASTTypeCon("B")))))
		val dtd2 = ASTDataTypeDefinition("B", Nil, List(ASTDataTypeConstructor("B", List(ASTTypeCon("A")))))
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd1, ModuleId("Test", "B") -> dtd2), nullDefs)
		val tA = TCon(Tycon(ModuleId("Test", "A"), Star))
		val tB = TCon(Tycon(ModuleId("Test", "B"), Star))
		defs.dcons should be === Map(
			ModuleId("Test", "A") -> (tB fn tA),
			ModuleId("Test", "B") -> (tA fn tB)
		)
	}

	it should "handle self-referential data constructors" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addTCon("A", ModuleId("Test", "A"))))
		val dtd = ASTDataTypeDefinition("A", Nil, List(ASTDataTypeConstructor("B", List(ASTTypeCon("A")))))
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		val tA = TCon(Tycon(ModuleId("Test", "A"), Star))
		defs.dcons should be === Map(
			ModuleId("Test", "B") -> (tA fn tA)
		)
	}

	it should "handle data constructors with quantified type variable arguments" in {
		val v = new ModuleVerifier(nullScopes)
		val dtd = ASTDataTypeDefinition("A", List("p", "q"), List(ASTDataTypeConstructor("B", List(ASTTypeVar("p"), ASTTypeVar("q")))))
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		val fi = Type.lastForallId
		val fa = Forall(fi, List(Star, Star), TGen(fi, 0) fn (TGen(fi, 1) fn TAp(TAp(TCon(Tycon(ModuleId("Test", "A"), Kfun(Star, Kfun(Star, Star)))), TGen(fi, 0)), TGen(fi, 1))))
		defs.dcons should be === Map(ModuleId("Test", "B") -> fa)
	}

	it should "infer the kind of type variables based upon their usage in data constructors" in {
		val v = new ModuleVerifier(nullScopes)
		val dtd = ASTDataTypeDefinition("A", List("p", "q"), List(ASTDataTypeConstructor("B", List(ASTTypeApply(ASTTypeVar("p"), List(ASTTypeVar("q")))))))
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		val fi = Type.lastForallId
		val fa = Forall(fi, List(Kfun(Star, Star), Star), TAp(TGen(fi, 0), TGen(fi, 1)) fn TAp(TAp(TCon(Tycon(ModuleId("Test", "A"), Kfun(Kfun(Star, Star), Kfun(Star, Star)))), TGen(fi, 0)), TGen(fi, 1)))
		defs.dcons should be === Map(ModuleId("Test", "B") -> fa)
	}

	it should "throw an error if the kind of type variables conflicts in the data constructors" in {
		val v = new ModuleVerifier(nullScopes)
		val dtd = ASTDataTypeDefinition("E", List("a", "b"), List(ASTDataTypeConstructor("X", List(ASTTypeApply(ASTTypeVar("a"), List(ASTTypeVar("b"))))), ASTDataTypeConstructor("Y", List(ASTTypeApply(ASTTypeVar("b"), List(ASTTypeVar("a")))))))
		evaluating { v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs) } should produce [KindConflictError]
	}

	it should "handle forall usage in data constructors" in {
		val v = new ModuleVerifier(nullScopes)
		val dtd = ASTDataTypeDefinition("A", Nil, List(ASTDataTypeConstructor("B", List(ASTForall(List("a"), ASTTypeVar("a"))))))
		val defs = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), nullDefs)
		val fi = Type.lastForallId
		val fa = Forall(fi, List(Star), TGen(fi, 0)) fn TCon(Tycon(ModuleId("Test", "A"), Star))
		defs.dcons should be === Map(ModuleId("Test", "B") -> fa)
	}

	it should "extend the tcons and dcons in the definitions list and leave all existing values unchanged" in {
		val v = new ModuleVerifier(nullScopes)
		val dtd = ASTDataTypeDefinition("A", Nil, List(ASTDataTypeConstructor("B", Nil)))
		val ModuleDefinitions(dcons, tcons, tcs, tcis, mts, mis) = v.addDataTypeDefs(Map(ModuleId("Test", "A") -> dtd), testDefs)
		dcons should be === testDefs.dcons + (ModuleId("Test", "A") -> TCon(Tycon(ModuleId("Test", "A"), Star)))
		tcons should be === testDefs.tcons + (ModuleId("Test", "B") -> TCon(Tycon(ModuleId("Test", "A"), Star)))
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
		val tc = ASTTypeClassDefinition("A", Nil, Nil, Nil)
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		} should produce [VerifierMiscError]
	}

	it should "produce typeclass definitions with single type parameters" in {
		val v = new ModuleVerifier(nullScopes)
		val tc = ASTTypeClassDefinition("A", Nil, List("a"), Nil)
		val defs = v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		defs.tcs should be === Map(
			ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty)
		)
	}

	it should "produce typeclass definitions with multiple type parameters" in {
		val v = new ModuleVerifier(nullScopes)
		val tc = ASTTypeClassDefinition("A", Nil, List("a", "b", "c"), Nil)
		val defs = v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		defs.tcs should be === Map(
			ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star), Tyvar("b", Star), Tyvar("c", Star)), Set.empty, Set.empty)
		)
	}

	it should "throw an error for superclass references to classes that are not in scope" in {
		val v = new ModuleVerifier(nullScopes)
		val tc = ASTTypeClassDefinition("B", List(ASTTypeClassReference("X", List("a"))), List("a"), Nil)
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		} should produce [UnknownTypeclassError]
	}

	it should "throw an error for typeclasses that pass undeclared type variables to a superclass" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))
			.addClass("B", ModuleId("Test", "B"))))
		val tcA = ASTTypeClassDefinition("A", Nil, List("p"), Nil)
		val tcB = ASTTypeClassDefinition("B", List(ASTTypeClassReference("A", List("z"))), List("q"), Nil)
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tcA, ModuleId("Test", "B") -> tcB), nullDefs)
		} should produce [UnknownTypeVariableError]
	}

	it should "throw an error for superclass references with the wrong arity" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))
			.addClass("B", ModuleId("Test", "B"))))
		val tcA = ASTTypeClassDefinition("A", Nil, List("p"), Nil)
		val tcB = ASTTypeClassDefinition("B", List(ASTTypeClassReference("A", List("x", "y"))), List("x", "y"), Nil)
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tcA, ModuleId("Test", "B") -> tcB), nullDefs)
		} should produce [TypeclassArityError]
	}

	it should "throw an error for recursive typeclass heirarchies" in {

		when("a typeclass has itself as a superclass")
		val v1 = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))))
		val tc = ASTTypeClassDefinition("A", List(ASTTypeClassReference("A", List("a"))), List("a"), Nil)
		//evaluating {
			v1.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		//} should produce [GraphError]

		when("a typeclass's superclass has the typeclass as a superclass")
		val v2 = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))
			.addClass("B", ModuleId("Test", "B"))))
		val tcA = ASTTypeClassDefinition("A", List(ASTTypeClassReference("B", List("a"))), List("a"), Nil)
		val tcB = ASTTypeClassDefinition("B", List(ASTTypeClassReference("A", List("b"))), List("b"), Nil)
		evaluating {
			v2.addTypeclassDefs(Map(ModuleId("Test", "A") -> tcA, ModuleId("Test", "B") -> tcB), nullDefs)
		} should produce [GraphError]
	}

	it should "produce typeclass definitions with a superclass" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))
			.addClass("B", ModuleId("Test", "B"))))
		val tcA = ASTTypeClassDefinition("A", Nil, List("a"), Nil)
		val tcB = ASTTypeClassDefinition("B", List(ASTTypeClassReference("A", List("b"))), List("b"), Nil)
		val defs = v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tcA, ModuleId("Test", "B") -> tcB), nullDefs)
		defs.tcs should be === Map(
			ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty),
			ModuleId("Test", "B") -> TypeclassDef(ModuleId("Test", "B"), List(IsIn(ModuleId("Test", "A"), List(TVar(Tyvar("b", Star))))), List(Tyvar("b", Star)), Set.empty, Set.empty)
		)
	}

	it should "produce typeclass definitions with multiple superclasses" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))
			.addClass("B", ModuleId("Test", "B"))
			.addClass("C", ModuleId("Test", "C"))))
		val tcA = ASTTypeClassDefinition("A", Nil, List("a"), Nil)
		val tcB = ASTTypeClassDefinition("B", Nil, List("b"), Nil)
		val tcC = ASTTypeClassDefinition("C", List(ASTTypeClassReference("A", List("c")), ASTTypeClassReference("B", List("c"))), List("c"), Nil)
		val defs = v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tcA, ModuleId("Test", "B") -> tcB, ModuleId("Test", "C") -> tcC), nullDefs)
		defs.tcs should be === Map(
			ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty),
			ModuleId("Test", "B") -> TypeclassDef(ModuleId("Test", "B"), Nil, List(Tyvar("b", Star)), Set.empty, Set.empty),
			ModuleId("Test", "C") -> TypeclassDef(ModuleId("Test", "C"), List(IsIn(ModuleId("Test", "A"), List(TVar(Tyvar("c", Star)))), IsIn(ModuleId("Test", "B"), List(TVar(Tyvar("c", Star))))), List(Tyvar("c", Star)), Set.empty, Set.empty)
		)
	}

	it should "produce typeclass definitions with multiple superclasses over multiple type variables" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))
			.addClass("B", ModuleId("Test", "B"))
			.addClass("C", ModuleId("Test", "C"))))
		val tcA = ASTTypeClassDefinition("A", Nil, List("a"), Nil)
		val tcB = ASTTypeClassDefinition("B", Nil, List("b"), Nil)
		val tcC = ASTTypeClassDefinition("C", List(ASTTypeClassReference("A", List("y")), ASTTypeClassReference("B", List("x"))), List("x", "y"), Nil)
		val defs = v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tcA, ModuleId("Test", "B") -> tcB, ModuleId("Test", "C") -> tcC), nullDefs)
		defs.tcs should be === Map(
			ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty),
			ModuleId("Test", "B") -> TypeclassDef(ModuleId("Test", "B"), Nil, List(Tyvar("b", Star)), Set.empty, Set.empty),
			ModuleId("Test", "C") -> TypeclassDef(ModuleId("Test", "C"), List(IsIn(ModuleId("Test", "A"), List(TVar(Tyvar("y", Star)))), IsIn(ModuleId("Test", "B"), List(TVar(Tyvar("x", Star))))), List(Tyvar("x", Star), Tyvar("y", Star)), Set.empty, Set.empty)
		)
	}

	it should "infer the kind of type variables using superclass information" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))
			.addClass("B", ModuleId("Test", "B"))))
		val defs0 = nullDefs.copy(tcs = (nullDefs.tcs + (ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Kfun(Star, Star))), Set.empty, Set.empty))))
		val tc = ASTTypeClassDefinition("B", List(ASTTypeClassReference("A", List("b"))), List("b"), Nil)
		val defs = v.addTypeclassDefs(Map(ModuleId("Test", "B") -> tc), defs0)
		defs.tcs should be === defs0.tcs +
			(ModuleId("Test", "B") -> TypeclassDef(ModuleId("Test", "B"), List(IsIn(ModuleId("Test", "A"), List(TVar(Tyvar("b", Kfun(Star, Star)))))), List(Tyvar("b", Kfun(Star, Star))), Set.empty, Set.empty))
	}

	it should "throw an error for typeclass members that don't use the class type variables" in {
		val v = new ModuleVerifier(nullScopes)

		when("the typeclass has one parameter")
		val tc1 = ASTTypeClassDefinition("A", Nil, List("a"), List(ASTTypeClassMemberDefinition("do-a", Nil, ASTTypeVar("b"))))
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc1), nullDefs)
		} should produce [TypeclassIllegalMemberDefinition]

		when("the typeclass has multiple parameters")
		val tc2 = ASTTypeClassDefinition("A", Nil, List("a", "b"), List(ASTTypeClassMemberDefinition("do-a", Nil, ASTTypeVar("a"))))
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc2), nullDefs)
		} should produce [TypeclassIllegalMemberDefinition]
	}

	it should "throw an error for duplicate typeclass members" in {
		val v = new ModuleVerifier(nullScopes)
		val tc = ASTTypeClassDefinition("A", Nil, List("a"), List(ASTTypeClassMemberDefinition("do-a", Nil, ASTTypeVar("a")), ASTTypeClassMemberDefinition("do-a", Nil, ASTTypeVar("a"))))
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		} should produce [TypeclassDuplicateMemberDefinitionError]
	}

	it should "throw an error for duplicate typeclass member implementations" in {
		val v = new ModuleVerifier(nullScopes)
		val tc = ASTTypeClassDefinition("A", Nil, List("a"), List(
			ASTTypeClassMemberDefinition("do-a", Nil, ASTFunctionType(List(ASTTypeVar("a"), ASTTypeVar("a")))),
			ASTTypeClassMemberImplementation("do-a", ASTFunction(List("a"), ASTValueRead("a"))),
			ASTTypeClassMemberImplementation("do-a", ASTFunction(List("a"), ASTValueRead("a")))))
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		} should produce [TypeclassDuplicateMemberImplementationError]
	}

	it should "throw an error for typeclass members implementations that have no corresponding definition"  in {
		val v = new ModuleVerifier(nullScopes)
		val tc = ASTTypeClassDefinition("A", Nil, List("a"), List(ASTTypeClassMemberImplementation("do-a", ASTFunction(List("a"), ASTValueRead("a")))))
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		} should produce [TypeclassImplementsUnknownMemberError]
	}

	it should "infer the kind of type variables using member definitions" in {
		val v = new ModuleVerifier(nullScopes)
		val tc = ASTTypeClassDefinition("A", Nil, List("a", "b"), List(ASTTypeClassMemberDefinition("do-a", Nil, ASTTypeApply(ASTTypeVar("a"), List(ASTTypeVar("b"))))))
		val defs = v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), nullDefs)
		defs.tcs should be === Map(
			ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Kfun(Star, Star)), Tyvar("b", Star)), Set("do-a"), Set.empty)
		)
	}

	it should "throw an error if the kind of type variables inferred from member definitions conflicts with the kind inferred from superclass information" in {
		val v = new ModuleVerifier(Map("Test" -> ImportedDefinitions.empty
			.addClass("A", ModuleId("Test", "A"))
			.addClass("B", ModuleId("Test", "B"))))
		val defs0 = nullDefs.copy(tcs = (nullDefs.tcs + (ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Kfun(Star, Star))), Set.empty, Set.empty))))
		val tc = ASTTypeClassDefinition("B", List(ASTTypeClassReference("A", List("b"))), List("b"), List(ASTTypeClassMemberDefinition("do-b", Nil, ASTTypeApply(ASTTypeVar("b"), List(ASTTypeVar("b"))))))
		evaluating {
			v.addTypeclassDefs(Map(ModuleId("Test", "B") -> tc), defs0)
		} should produce [KindConflictError]
	}

	it should "extend the tcs in the definitions list and leave all existing values unchanged" in {
		val v = new ModuleVerifier(nullScopes)
		val tc = ASTTypeClassDefinition("A", Nil, List("a"), Nil)
		val ModuleDefinitions(dcons, tcons, tcs, tcis, mts, mis) = v.addTypeclassDefs(Map(ModuleId("Test", "A") -> tc), testDefs)
		dcons should be === testDefs.dcons
		tcons should be === testDefs.tcons
		tcs should be === testDefs.tcs + (ModuleId("Test", "A") -> TypeclassDef(ModuleId("Test", "A"), Nil, List(Tyvar("a", Star)), Set.empty, Set.empty))
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
	ignore should "produce typeclass instances with members that make use of member-specific predicates" in {
		/*
		; (class Ohno (a)
		; (def ohno (=> (Eq b) (-> a b String))))

		; (instance Ohno ((List a))
		; (let ohno (lambda (xs z)
		; ; TODO: typing fails with this present at the moment,
		; ; instance member type is missing the (Eq b) context from the tc def
		; ; (== z z)
		; "foo")))*/
	}
	ignore should "extend the tcis in the definitions list and leave all existing values unchanged" in {}

	// ------------------------------------------------------------------------

	behavior of "addMemberDefs"
	ignore should "throw an error if a member is defined more than once"
	ignore should "throw an error if a member name overlaps with a typeclass member name"
	ignore should "extend the mts in the definitions list and leave all existing values unchanged" in {}

	// ------------------------------------------------------------------------

	behavior of "addTypeclassMemberDefs"
	ignore should "handle members with additional typeclass predicates" in {}
	ignore should "throw an error if a member has additional typeclass predicates that cause a kind mismatch" in {}
	ignore should "extend the mts in the definitions list and leave all existing values unchanged" in {}

	// ------------------------------------------------------------------------

	behavior of "addMemberImplementations"
	ignore should "check that there is an implementation for each module-defined member" in {}
	ignore should "do some stuff" in {}

	// ------------------------------------------------------------------------

	behavior of "getMemberType"
	ignore should "do some stuff" in {}

	// ------------------------------------------------------------------------

	behavior of "getPredicates"
	ignore should "do some stuff" in {}

	// ------------------------------------------------------------------------

	behavior of "lookupInstanceType"
	ignore should "do some stuff" in {}
}
