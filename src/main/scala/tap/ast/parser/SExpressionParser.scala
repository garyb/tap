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

    //  [ lexer-like rules ]  -----------------------------------------------------------------------------------------

    val ID = regex("""[^A-Z0-9\(\[\)\]#\s'][^\(\[\)\]\s]*"""r)
    val TYPEID = regex("""[^a-z\(\[\)\]#\s'][^\(\[\)\]\s]*"""r)
    val TYPEVARID = regex("""[a-z][a-zA-Z0-9_]*"""r)

    val NUM = ( "infinity" ^^ { case num => java.lang.Double.POSITIVE_INFINITY }
              | regex("""0x[0-9a-fA-F]*\b"""r) ^^ { case num => java.lang.Long.parseLong(num.drop(2), 16).toDouble }
              | regex("""#[0-9a-fA-F]*\b"""r) ^^ { case num => java.lang.Long.parseLong(num.drop(1), 16).toDouble }
              | regex("""\-?[0-9]+(\.[0-9]+)?(e[0-9]+)?\b"""r) ^^ { case num => java.lang.Double.parseDouble(num) }
              )

    val STR = regex(""""(\\"|[^"])*""""r) ^^ { case str => str.drop(1).dropRight(1).replace("""\"""", "\"") }

    val reserved = "module" | "import" | "lambda" | "let" | "letrec" | "infinity" | "begin" | "if" | "match" | "case" | "def" | "data" | "type" | "class" | "instance" | "native" | "->" | "=>" | "error" | "and" | "or"
    val id = not(reserved) ~> ID
    val typeId = not(reserved) ~> TYPEID
    val classId = not(reserved) ~> TYPEID
    val tvarId = not(reserved | "forall") ~> TYPEVARID
    val moduleId = typeId

    //  [ values ]  ---------------------------------------------------------------------------------------------------

    val number = sourced( NUM ^^ { case v => ASTNumber(v) } )
    val string = sourced( STR ^^ { case v => ASTString(v) } )
    val native = sourced( "native" ^^^ ASTNativeValue )
    val wildcard = sourced( "_" ^^^ ASTWildcardValue )
    val ref = sourced( id ^^ { case v => ASTValueRead(v) } )

    val tuple = sourced( "'" ~> "(" ~> (expr+) <~ ")" ^^ { case values => makeTuple(values) } )
    val array = sourced( "[" ~> (expr*) <~ "]" ^^ { case values => makeArray(values) } )

    val dcon = sourced( typeId ^^ { case id => ASTValueRead(id) } )
    val instantiate = sourced( typeId ^^ { case id => ASTValueRead(id) }
                             | "(" ~> dcon ~ (expr+) <~ ")"  ^^ { case dcon ~ exprs => ASTApply(dcon, exprs) }
                             )

    val value: Parser[ASTValue] = number | string | native | wildcard | ref

    //  [ data types ]  -----------------------------------------------------------------------------------------------

    val dataType = sourced( "(" ~> "data" ~> typeId ~ (dataConstructor*) <~ ")" ^^ { case id ~ constructors => ASTDataType(id, List.empty, constructors) }
                          | "(" ~> "data" ~> "(" ~> typeId ~ (tvarId+) ~ ")" ~ (dataConstructor*) <~ ")" ^^ { case id ~ params ~ _ ~ constructors => ASTDataType(id, params, constructors) }
                          )

    val dataConstructor = sourced( typeId ^^ { case id => ASTDataCon(id, List.empty) }
                                 | "(" ~> typeId ~ (typeRef+) <~ ")" ^^ { case id ~ args => ASTDataCon(id, args) }
                                 )

    //  [ typeclasses ]  ----------------------------------------------------------------------------------------------

    val typeclass = sourced( "(" ~> "class" ~> classId ~ "(" ~ (tvarId+) ~ ")" ~ (typeclassMember*) <~ ")" ^^ { case id ~ _ ~ params ~ _ ~ members => ASTClass(id, List.empty, params, members)}
                           | "(" ~> "class" ~> classId ~ "(" ~ "=>" ~ (typeclassRef+) ~ ")" ~ "(" ~ (tvarId+) ~ ")" ~ (typeclassMember*) <~ ")" ^^ { case id ~ _ ~ _ ~ context ~ _ ~ _  ~ params ~ _ ~ members => ASTClass(id, context, params, members)}
                           )

    val typeclassRef = sourced( "(" ~> classId ~ (tvarId+) <~ ")" ^^ { case id ~ params => ASTClassRef(id, params) } )

    val typeclassMember = sourced( "(" ~> "def" ~> id ~ qualTypeRef <~ ")" ^^ { case id ~ qtype => ASTClassMemberDef(id, qtype) }
                                 | typeclassMemberImplementation
                                 )

    val typeclassMemberImplementation = sourced( "(" ~> "let" ~> id ~ expr <~ ")" ^^ { case id ~ expr => ASTClassMemberImpl(id, expr) } )

    val instance = sourced( "(" ~> "instance" ~> classId ~ "(" ~ "=>" ~ (typeclassRef+) ~ ")" ~ "(" ~ (typeRef+) ~ ")" ~ (typeclassMemberImplementation*) <~ ")" ^^ { case id ~ _ ~ _ ~ context ~ _ ~ _ ~ params ~ _ ~ members => ASTClassInst(id, context, params, members) }
                          | "(" ~> "instance" ~> classId ~ "(" ~ (typeRef+) ~ ")" ~ (typeclassMemberImplementation*) <~ ")" ^^ { case id ~ _ ~ params ~ _ ~ members => ASTClassInst(id, List.empty, params, members) }
                          )

    //  [ references to types ]  --------------------------------------------------------------------------------------

    val tconRef = sourced( typeId ^^ { case id => ASTTypeCon(id) } )
    val tvarRef = sourced( tvarId ^^ { case id => ASTTypeVar(id) } )
    val typeVarRef = sourced( tvarId ^^ { case id => ASTTypeVar(id) } )
    val funcTypeRef = sourced( "(" ~> "->" ~> (typeRef+) <~ ")" ^^ { case params => ASTFunctionType(params) }
                             | "->" ^^ { case _ => ASTTypeCon("->") }
                             )
    val forallTypeRef = sourced( "(" ~> "forall" ~> "(" ~> (tvarId+) ~ ")" ~ typeRef <~ ")" ^^ { case qvs ~ _ ~ t => ASTForall(qvs, t) } )

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

    val qualTypeRef = sourced( typeRef ^^ { case ttype => ASTQType(List.empty, ttype) }
                             | "(" ~> "=>" ~> (typeclassRef+) ~ ")" ~ typeRef ^^ { case ctx ~ _ ~ ttype => ASTQType(ctx, ttype) }
                             )

    //  [ special forms ]  --------------------------------------------------------------------------------------------

    val define = sourced( "(" ~> "def" ~> id ~ qualTypeRef <~ ")" ^^ { case id ~ qtype => ASTDef(id, qtype) } )

    val declare = sourced( "(" ~> "let" ~> id ~ expr <~ ")" ^^ { case id ~ term => ASTLet(id, term) } )

    val block = sourced( "(" ~> "begin" ~> (expr+) <~ ")" ^^ { case exprList => ASTBlock(exprList) } )

    val func = sourced( "(" ~> "lambda" ~> "(" ~> (id*) ~ ")" ~ (expr+) <~ ")" ^^ {  case args ~ _ ~ body => ASTFunction(args, maybeBlock(body)) } )

    val op = sourced( "(" ~> "and" ~> expr ~ (expr+) <~ ")" ^^ { case term ~ terms => makeAnd(term :: terms) }
                    | "(" ~> "or" ~> expr ~ (expr+) <~ ")" ^^ { case term ~ terms => makeOr(term :: terms) }
                    )

    val branch = sourced( "(" ~> "if" ~> ((expr ~ expr)+) ~ expr <~ ")" ^^ { case condBranches ~ branchElse => makeIf(condBranches, branchElse) } )

    val matchForm = sourced( "(" ~> "match" ~> expr ~ (matchCase+) <~ ")" ^^ { case expr ~ cases => ASTMatch(expr, cases) } )
    val matchCase = sourced( "(" ~> "case" ~> matchValue ~ expr <~ ")" ^^ { case value ~ expr => ASTCaseBranch(value, None, expr) }
                           | "(" ~> "case-if" ~> matchValue ~ expr ~ expr <~ ")" ^^ { case value ~ guard ~ expr => ASTCaseBranch(value, Some(guard), expr) }
                           )

    val matchValue: Parser[ASTPattern] = number | string | tupleDestructure | wildcard | ref | datatypeDestructure | matchCaseBind
    val matchCaseBind = sourced( "[" ~> id ~ matchValue <~ "]" ^^ { case name ~ value => ASTBind(name, value) } )
    val tupleDestructure = sourced( "'" ~> "(" ~> (matchValue+) <~ ")" ^^ { case values => ASTUnapply("Tuple" + values.size, values) } )
    val datatypeDestructure = sourced( typeId ^^ { case id => ASTUnapply(id, List.empty) }
                                     | "(" ~> typeId ~ (matchValue+) <~ ")"  ^^ { case id ~ values => ASTUnapply(id, values) }
                                     )

    val error = sourced( "(" ~> "error" ~> expr <~ ")" ^^ { case expr => ASTRaiseError(expr) } )
    val cast = sourced( "(" ~> "cast" ~> expr ~ typeRef <~ ")" ^^ { case expr ~ ttype => ASTCast(expr, ttype) } )

    val special = declare | block | func | tuple | array | op | branch | matchForm | error | cast

    //  [ expressions ]  ----------------------------------------------------------------------------------------------

    val expr: Parser[ASTExpr] = special | value | instantiate | call
    val call = sourced( "(" ~> (expr+) <~ ")" ^^ { case exprs => ASTApply(exprs.head, exprs.tail) } )

    //  [ modules ]  --------------------------------------------------------------------------------------------------

    val withPrefix = "(" ~> "with-prefix" ~> id <~ ")"
    val moduleMemberImport = id | typeId | classId

    val mimport = sourced( "(" ~> "import" ~> moduleId ~ (withPrefix?) <~ ")" ^^ { case id ~ prefix => ASTImport(id, None, prefix) }
                         | "(" ~> "import" ~> "(" ~> moduleId ~ (moduleMemberImport+) ~ ")" ~ (withPrefix?) <~ ")" ^^ { case id ~ defs ~ _ ~ prefix => ASTImport(id, Some(defs.toSet), prefix) }
                         )

    val exports = sourced( "(" ~> "export" ~> "(" ~> "data" ~> typeId ~ "(" ~ (typeId*) <~ ")" <~ ")" <~ ")" ^^ { case tcon ~ _ ~ dcons => ASTDataTypeExport(tcon, dcons.toSet) }
                         | "(" ~> "export" ~> "(" ~> "class" ~> classId <~ ")" <~ ")" ^^ { case id => ASTClassExport(id) }
                         | "(" ~> "export" ~> "(" ~> "module" ~> moduleId <~ ")" <~ ")" ^^ { case id => ASTModuleExport(id) }
                         | "(" ~> "export" ~> id <~ ")" ^^ { case id => ASTMemberExport(id) }
                         )

    val moduleDefinition = mimport | exports | define | dataType | typeclass | instance | declare

    //  [ parser behaviour ]  -----------------------------------------------------------------------------------------

    val exportId = id | typeId
    val program = "(" ~> "module" ~> moduleId ~ ")" ~ (moduleDefinition*) ^^ { case module ~ _ ~ members => ASTModule(module, members) }

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

    def apply[T <: ASTNode](file: String, input: CharSequence, parser: Parser[T] = program): T = {
        currFile = file
        parseAll(parser, input) match {
            case Success(result, _) => result
            case NoSuccess(msg, next) => throw new SExpressionParseException(msg, next.pos)
        }
    }
}