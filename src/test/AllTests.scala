package test

import org.scalatest.Suites

class AllTests extends Suites(

	new util.ContextOpTests,

	new ast.ASTUtilTests,
	new ast.parser.ParserTests,

	new ir.TapNodeTests,
	new ir.TapNodeUtilTests,

	new types.TypeTests,
	new types.classes.ClassEnvironmentsTests,
	new types.classes.IsInTests,
	new types.classes.QualTests,
	new types.inference.SubstitutionsTests,
	new types.inference.UnifyTests,
	new types.inference.TypeInferenceTests,

	new verifier.ModuleVerifierTests,

	new interpreter.InterpreterTests
)