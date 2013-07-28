package test

import org.scalatest.Suites

class AllTests extends Suites(

    new util.ContextOpTests,
    new util.GraphTests,

    new ast.ASTUtilTests,
    new ast.parser.ParserTests,

    new ir.TapNodeTests,
    new ir.TapNodeUtilTests,

    new types.TypeTests,
    new types.kinds.KindTests,
    new types.classes.ClassEnvironmentsTests,
    new types.classes.IsInTests,
    new types.classes.QualTests,
    new types.inference.SubstitutionsTests,
    new types.inference.UnifyTests,
    new types.inference.TypeInferenceTests,

    new verifier.ModuleVerifierTests,
    new verifier.ModuleTypeInferenceTests,
    new verifier.ProgramVerifierTests,
    new verifier.TypeclassInliningTests,

    new interpreter.InterpreterTests
)