package tap.ast.parser

import tap.ast._
import util.parsing.combinator.RegexParsers
import language.postfixOps
import language.implicitConversions

/**
 * S-expression-like syntax parser for Tap.
 */
object SExpressionParser extends RegexParsers {

    def isTerminator(s: CharSequence): Boolean =
        (whiteSpace findPrefixOf s) != None || s == "(" || s == ")" || s == "[" || s == "]"

    override implicit def literal(s: String): Parser[String] = new Parser[String] {
        def apply(in: Input) = {
            val source = in.source
            val offset = in.offset
            val start = handleWhiteSpace(source, offset)
            var i = 0
            var j = start
            while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
                i += 1
                j += 1
            }
            if (i == s.length && (isTerminator(s) || source.length() == j || isTerminator(source.subSequence(j, j + 1)))) {
                Success(source.subSequence(start, j).toString, in.drop(j - offset))
            } else {
                val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
                Failure("`"+s+"' expected but "+found+" found", in.drop(start - offset))
            }
        }
    }

    //  [ helper functions ]  -----------------------------------------------------------------------------------------

    def maybeBlock(list: List[ASTExpr]): ASTExpr = if (list.length == 1) list(0) else ASTBlock(list).setPos(list(0).pos)

    def makeTuple(values: List[ASTExpr]): ASTExpr =
        ASTApply(ASTValueRead("Tuple" + values.length), values)

    def makeArray(values: List[ASTExpr]): ASTExpr =
        values.foldRight(ASTValueRead("EOL"): ASTExpr) { (x, y) => ASTApply(ASTValueRead(":"), List(x, y)) }

    def makeAnd(xs: List[ASTExpr]): ASTExpr =
        xs.foldRight(ASTValueRead("True"): ASTExpr) { (e, inner) => ASTMatch(e, List(ASTCaseBranch(ASTValueRead("True"), None, inner), ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("False")))) }

    def makeOr(xs: List[ASTExpr]): ASTExpr =
        xs.foldRight(ASTValueRead("False"): ASTExpr) { (e, inner) => ASTMatch(e, List(ASTCaseBranch(ASTValueRead("False"), None, inner), ASTCaseBranch(ASTWildcardValue, None, ASTValueRead("True")))) }

    def makeIf(condBranches: List[ASTExpr ~ ASTExpr], elseBranch: ASTExpr): ASTExpr = {
        val mcs = condBranches map { case (cond ~ branch) =>
            ASTCaseBranch(ASTWildcardValue, Some(cond), branch)
        }
        ASTMatch(ASTValueRead("Unit"), mcs :+ ASTCaseBranch(ASTWildcardValue, None, elseBranch))
    }

    def createModule(name: String, members: List[ASTModuleMember]) = {
        val imports = members.collect { case ASTImport(id) => id }
        val exports = members.collect {
            case ASTDataTypeDefinition(id, _, dcons) => ExDataType(id, (dcons map { dcon => dcon.name }).toSet)
            case ASTLet(id, _) => ExMember(id)
            case ASTModuleExport(id) => ExModule(id)
        } ++ (members.collect {
            case ASTTypeClassDefinition(id, _, _, ms) => ExClass(id) :: (ms map { m => ExMember(m.name) })
        }.flatten)
        val datatypes = members.collect { case ast: ASTDataTypeDefinition => ast }
        val typeclasses = members.collect { case ast: ASTTypeClassDefinition => ast }
        val instances = members.collect { case ast: ASTTypeClassInstance => ast }
        val memberDefs = members.collect { case ast: ASTDef => ast }
        val memberImpls = members.collect { case ast: ASTLet => ast }
        ASTModule(name, exports.toSet, imports.toSet, datatypes, typeclasses, instances, memberDefs, memberImpls)
    }

    //  [ lexer-like rules ]  -----------------------------------------------------------------------------------------

    val ID = regex("""[^A-Z0-9\(\[\)\]#\s'][^\(\[\)\]\s]*"""r)
    val TYPEID = regex("""[^a-z\(\[\)\]#\s'][^\(\[\)\]\s]*"""r)
    val PARAMID = regex("""[a-z][a-zA-Z0-9_]*"""r)

    val NUM = ( "infinity" ^^ { case num => java.lang.Double.POSITIVE_INFINITY }
              | regex("""0x[0-9a-fA-F]*\b"""r) ^^ { case num => java.lang.Long.parseLong(num.drop(2), 16).toDouble }
              | regex("""#[0-9a-fA-F]*\b"""r) ^^ { case num => java.lang.Long.parseLong(num.drop(1), 16).toDouble }
              | regex("""\-?[0-9]+(\.[0-9]+)?(e[0-9]+)?\b"""r) ^^ { case num => java.lang.Double.parseDouble(num) }
              )

    val STR = regex(""""(\\"|[^"])*""""r) ^^ { case str => str.drop(1).dropRight(1).replace("""\"""", "\"") }

    val reserved = "module" | "import" | "lambda" | "let" | "letrec" | "infinity" | "begin" | "if" | "match" | "case" | "def" | "data" | "type" | "class" | "instance" | "native" | "->" | "=>" | "error" | "and" | "or"
    val ident = not(reserved) ~> ID
    val typeIdent = not(reserved) ~> TYPEID
    val classIdent = not(reserved) ~> TYPEID
    val paramIdent = not(reserved | "forall") ~> PARAMID
    val moduleIdent = typeIdent

    //  [ values ]  ---------------------------------------------------------------------------------------------------

    val number = sourced( NUM ^^ { case v => ASTNumber(v) } )
    val string = sourced( STR ^^ { case v => ASTString(v) } )
    val native = sourced( "native" ^^^ ASTNativeValue )
    val wildcard = sourced( "_" ^^^ ASTWildcardValue )
    val ref = sourced( ident ^^ { case v => ASTValueRead(v) } )

    val tuple = sourced( "'" ~> "(" ~> (expr+) <~ ")" ^^ { case values => makeTuple(values) } )
    val array = sourced( "[" ~> (expr*) <~ "]" ^^ { case values => makeArray(values) } )

    val dcon = sourced( typeIdent ^^ { case ident => ASTValueRead(ident) } )
    val instantiate = sourced( typeIdent ^^ { case ident => ASTValueRead(ident) }
                                | "(" ~> dcon ~ (expr+) <~ ")"  ^^ { case dcon ~ exprs => ASTApply(dcon, exprs) }
                                )

    val value: Parser[ASTValue] = number | string | native | wildcard | ref

    //  [ data types ]  -----------------------------------------------------------------------------------------------

    val dataType = sourced( "(" ~> "data" ~> typeIdent ~ (typeConstructor*) <~ ")" ^^ { case ident ~ constructors => ASTDataTypeDefinition(ident, List.empty, constructors) }
                             | "(" ~> "data" ~> "(" ~> typeIdent ~ (ident+) ~ ")" ~ (typeConstructor*) <~ ")" ^^ { case ident ~ params ~ _ ~ constructors => ASTDataTypeDefinition(ident, params, constructors) }
                             )

    val typeConstructor = sourced( typeIdent ^^ { case ident => ASTDataTypeConstructor(ident, List.empty) }
                                    | "(" ~> typeIdent ~ (typeRef+) <~ ")" ^^ { case ident ~ args => ASTDataTypeConstructor(ident, args) }
                                    )

    //  [ typeclasses ]  ----------------------------------------------------------------------------------------------

    val typeclass = sourced( "(" ~> "class" ~> classIdent ~ "(" ~ (paramIdent+) ~ ")" ~ (typeclassMember*) <~ ")" ^^ { case ident ~ _ ~ params ~ _ ~ members => ASTTypeClassDefinition(ident, List.empty, params, members)}
                              | "(" ~> "class" ~> classIdent ~ "(" ~ "=>" ~ (typeclassRef+) ~ ")" ~ "(" ~ (paramIdent+) ~ ")" ~ (typeclassMember*) <~ ")" ^^ { case ident ~ _ ~ _ ~ context ~ _ ~ _  ~ params ~ _ ~ members => ASTTypeClassDefinition(ident, context, params, members)}
                              )

    val typeclassRef = sourced( "(" ~> classIdent ~ (paramIdent+) <~ ")" ^^ { case ident ~ params => ASTTypeClassReference(ident, params) } )

    val typeclassMember = sourced( "(" ~> "def" ~> ident ~ typeRef <~ ")" ^^ { case ident ~ ttype => ASTTypeClassMemberDefinition(ident, List.empty, ttype) }
                                    | "(" ~> "def" ~> ident ~ "(" ~ "=>" ~ (typeclassRef+) ~ typeRef <~ ")" <~ ")" ^^ { case id ~ _ ~ _ ~ context ~ ttype => ASTTypeClassMemberDefinition(id, context, ttype) }
                                    | typeclassMemberImplementation
                                    )

    val typeclassMemberImplementation = sourced( "(" ~> "let" ~> ident ~ expr <~ ")" ^^ { case ident ~ expr => ASTTypeClassMemberImplementation(ident, expr) } )

    val instance = sourced( "(" ~> "instance" ~> classIdent ~ "(" ~ "=>" ~ (typeclassRef+) ~ ")" ~ "(" ~ (typeRef+) ~ ")" ~ (typeclassMemberImplementation*) <~ ")" ^^ { case ident ~ _ ~ _ ~ context ~ _ ~ _ ~ params ~ _ ~ members => ASTTypeClassInstance(ident, context, params, members) }
                             | "(" ~> "instance" ~> classIdent ~ "(" ~ (typeRef+) ~ ")" ~ (typeclassMemberImplementation*) <~ ")" ^^ { case ident ~ _ ~ params ~ _ ~ members => ASTTypeClassInstance(ident, List.empty, params, members) }
                             )

    //  [ references to types ]  --------------------------------------------------------------------------------------

    val tconRef = sourced( typeIdent ^^ { case ident => ASTTypeCon(ident) } )
    val tvarRef = sourced( paramIdent ^^ { case ident => ASTTypeVar(ident) } )
    val typeVarRef = sourced( paramIdent ^^ { case ident => ASTTypeVar(ident) } )
    val funcTypeRef = sourced( ( "(" ~> "->" ~> (typeRef+) <~ ")" ^^ { case params => ASTFunctionType(params) }
                                  | "->" ^^ { case _ => ASTTypeCon("->") } ))
    val forallTypeRef = sourced( "(" ~> "forall" ~> "(" ~> (paramIdent+) ~ ")" ~ typeRef <~ ")" ^^ { case qvs ~ _ ~ t => ASTForall(qvs, t) } )

    val typeRef: Parser[ASTType] =
        ( forallTypeRef
        | tconRef
        | sourced( "(" ~> tconRef ~ (typeRef+) <~ ")" ^^ { case tcon ~ params => ASTTypeApply(tcon, params) } )
        | typeVarRef
        | sourced( "(" ~> tvarRef ~ (typeRef+) <~ ")" ^^ { case tvar ~ params => ASTTypeApply(tvar, params) } )
        | funcTypeRef
        | sourced( "'" ~> "(" ~> (typeRef+) <~ ")" ^^ { case params => ASTTypeApply(ASTTypeCon("Tuple" + params.size), params) }  )
        | sourced( "[" ~ "]" ^^^ ASTTypeCon("List") )
        | sourced( "[" ~> typeRef <~ "]" ^^ { case t => ASTTypeApply(ASTTypeCon("List"), List(t)) })
        )

    //  [ special forms ]  --------------------------------------------------------------------------------------------

    val define = sourced( "(" ~> "def" ~> ident ~ typeRef <~ ")" ^^ { case id ~ ttype => ASTDef(id, List.empty, ttype) }
                           | "(" ~> "def" ~> ident ~ "(" ~ "=>" ~ (typeclassRef+) ~ typeRef <~ ")" <~ ")" ^^ { case id ~ _ ~ _ ~ context ~ ttype => ASTDef(id, context, ttype) }
                           )

    val declare = sourced( "(" ~> "let" ~> ident ~ expr <~ ")" ^^ { case id ~ term => ASTLet(id, term) } )

    val block = sourced( "(" ~> "begin" ~> (expr+) <~ ")" ^^ { case exprList => ASTBlock(exprList) } )

    val func = sourced( "(" ~> "lambda" ~> "(" ~> (ident*) ~ ")" ~ (expr+) <~ ")" ^^ {  case args ~ _ ~ body => ASTFunction(args, maybeBlock(body)) } )

    val op = sourced( "(" ~> "and" ~> expr ~ (expr+) <~ ")" ^^ { case term ~ terms => makeAnd(term :: terms) }
                       | "(" ~> "or" ~> expr ~ (expr+) <~ ")" ^^ { case term ~ terms => makeOr(term :: terms) } )

    val branch = sourced( "(" ~> "if" ~> ((expr ~ expr)+) ~ expr <~ ")" ^^ { case condBranches ~ branchElse => makeIf(condBranches, branchElse) } )

    val matchForm = sourced( "(" ~> "match" ~> expr ~ (matchCase+) <~ ")" ^^ { case expr ~ cases => ASTMatch(expr, cases) } )
    val matchCase = sourced( "(" ~> "case" ~> matchValue ~ expr <~ ")" ^^ { case value ~ expr => ASTCaseBranch(value, None, expr) }
                              | "(" ~> "case-if" ~> matchValue ~ expr ~ expr <~ ")" ^^ { case value ~ guard ~ expr => ASTCaseBranch(value, Some(guard), expr) }
                              )

    val matchValue: Parser[ASTPattern] = number | string | tupleDestructure | wildcard | ref | datatypeDestructure | matchCaseBind
    val matchCaseBind = sourced( "[" ~> ident ~ matchValue <~ "]" ^^ { case name ~ value => ASTBind(name, value) } )
    val tupleDestructure = sourced( "'" ~> "(" ~> (matchValue+) <~ ")" ^^ { case values => ASTUnapply("Tuple" + values.size, values) } )
    val datatypeDestructure = sourced( typeIdent ^^ { case ident => ASTUnapply(ident, List.empty) }
                                        | "(" ~> typeIdent ~ (matchValue+) <~ ")"  ^^ { case ident ~ values => ASTUnapply(ident, values) }
                                        )

    val error = sourced( "(" ~> "error" ~> expr <~ ")" ^^ { case expr => ASTRaiseError(expr) } )
    val cast = sourced( "(" ~> "cast" ~> expr ~ typeRef <~ ")" ^^ { case expr ~ ttype => ASTCast(expr, ttype) } )

    val special = declare | block | func | tuple | array | op | branch | matchForm | error | cast

    //  [ expressions ]  ----------------------------------------------------------------------------------------------

    val expr: Parser[ASTExpr] = special | value | instantiate | call
    val call = sourced( "(" ~> (expr+) <~ ")" ^^ { case exprs => ASTApply(exprs.head, exprs.tail) } )

    //  [ modules ]  --------------------------------------------------------------------------------------------------

    val mimport = sourced( "(" ~> "import" ~> moduleIdent <~ ")" ^^ { case ident => ASTImport(ident) } )
    val mexport = sourced( "(" ~> "export" ~> "(" ~> "module" ~> moduleIdent <~ ")" <~ ")" ^^ { case ident => ASTModuleExport(ident) } )

    val moduleDefinition = mimport | mexport | define | dataType | typeclass | instance | declare

    //  [ parser behaviour ]  -----------------------------------------------------------------------------------------

    val exportIdent = ident | typeIdent
    val program = "(" ~> "module" ~> moduleIdent ~ ")" ~ (moduleDefinition*) ^^ { case module ~ _ ~ members => createModule(module, members) }

    protected val comments = """(;[^\n]*[\s\r\n]*)+"""r

    override protected def handleWhiteSpace(source: CharSequence, offset: Int): Int =
        if (skipWhitespace) {
            val newOffset = (whiteSpace findPrefixMatchOf (source.subSequence(offset, source.length))) match {
                case Some(matched) => offset + matched.end
                case None => offset
            }
            (comments findPrefixMatchOf (source.subSequence(newOffset, source.length))) match {
                case Some(matched) => newOffset + matched.end
                case None => newOffset
            }
        } else offset

    def sourced[T <: FilePositional](p: => Parser[T]): Parser[T] = positioned(Parser { in =>
        p(in) match {
            case Success(t, in1) => Success(if (t.file == null) t setFile currFile else t, in1)
            case ns: NoSuccess => ns
        }
    })

    var currFile: String = null

    def apply[T <: ASTNode](file: String, input: String, parser: Parser[T] = program): T = {
        currFile = file
        parseAll(parser, input) match {
            case Success(result, _) => result
            case NoSuccess(msg, next) => throw new SExpressionParseException(msg, next.pos)
        }
    }
}