package tap.ast.parser

import util.parsing.input.Position


class SExpressionParseException(msg: String, pos: Position) extends
	Exception("Parsing failed: " + msg + " (line " + pos.line + ", col " + pos.column + "):\n" + pos.longString)