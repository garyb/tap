package test

import tap.ast.parser.SExpressionParser
import tap.ast.{ASTType, ASTExpr, ASTModule}

trait ParserFixture {
	def parseModule(input: String): ASTModule = SExpressionParser("test", input)
	def parseExpr(input: String): ASTExpr = SExpressionParser("test", input, SExpressionParser.expr)
	def parseType(input: String): ASTType = SExpressionParser("test", input, SExpressionParser.typeRef)
}
