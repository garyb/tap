package tap.ast

sealed trait ASTNode extends FilePositional

// ---[ module structure ]---------------------------------------------------------------------------------------------

case class ASTModule(name: String, members: List[ASTModuleMember]) extends ASTNode

// ---[ module-level ]-------------------------------------------------------------------------------------------------

sealed trait ASTModuleMember extends ASTNode
case class ASTImport(module: String, defs: Option[Set[String]], prefix: Option[String]) extends ASTModuleMember

sealed trait ASTExport extends ASTModuleMember
case class ASTDataTypeExport(name: String, dcons: Set[String]) extends ASTExport
case class ASTClassExport(name: String) extends ASTExport
case class ASTMemberExport(name: String) extends ASTExport
case class ASTModuleExport(name: String) extends ASTExport

case class ASTDef(name: String, qtype: ASTQType) extends ASTModuleMember

// ---[ expressions ]--------------------------------------------------------------------------------------------------

sealed trait ASTExpr extends ASTNode
case class ASTBlock(children: List[ASTExpr]) extends ASTExpr
case class ASTApply(func: ASTExpr, args: List[ASTExpr]) extends ASTExpr
case class ASTMatch(expr: ASTExpr, cases: List[ASTCaseBranch]) extends ASTExpr
case class ASTLet(name: String, value: ASTExpr) extends ASTExpr with ASTModuleMember
case class ASTValueRead(name: String) extends ASTValue with ASTPattern
case class ASTCast(expr: ASTExpr, ttype: ASTType) extends ASTExpr
case class ASTRaiseError(expr: ASTExpr) extends ASTExpr

// ---[ match cases & patterns ]---------------------------------------------------------------------------------------

case class ASTCaseBranch(value: ASTPattern, guard: Option[ASTExpr], expr: ASTExpr) extends ASTNode

sealed trait ASTPattern extends ASTNode
case class ASTBind(name: String, value: ASTPattern) extends ASTPattern
case class ASTUnapply(dconName: String, params: List[ASTPattern]) extends ASTPattern

// ---[ values ]-------------------------------------------------------------------------------------------------------

sealed trait ASTValue extends ASTExpr
case class ASTString(value: String) extends ASTValue with ASTPattern
case class ASTNumber(value: Double) extends ASTValue with ASTPattern
case class ASTFunction(args: List[String], body: ASTExpr) extends ASTValue
case object ASTWildcardValue extends ASTValue with ASTPattern
case object ASTNativeValue extends ASTValue

// ---[ types ]--------------------------------------------------------------------------------------------------------

case class ASTQType(context: List[ASTTypeClassReference], ttype: ASTType) extends ASTNode

sealed abstract class ASTType extends ASTNode
case class ASTTypeCon(name: String) extends ASTType
case class ASTTypeVar(name: String) extends ASTType
case class ASTTypeApply(ttype: ASTType, params: List[ASTType]) extends ASTType
case class ASTFunctionType(params: List[ASTType]) extends ASTType
case class ASTForall(scope: List[String], ttype: ASTType) extends ASTType

// ---[ data types ]---------------------------------------------------------------------------------------------------

case class ASTDataTypeDefinition(name: String, params: List[String], constructors: List[ASTDataTypeConstructor]) extends ASTModuleMember
case class ASTDataTypeConstructor(name: String, args: List[ASTType]) extends ASTNode

// ---[ typeclasses ]--------------------------------------------------------------------------------------------------

case class ASTTypeClassReference(name: String, params: List[String]) extends ASTNode

case class ASTTypeClassDefinition(name: String,
                                  context: List[ASTTypeClassReference],
                                  params: List[String],
                                  members: List[ASTTypeClassMember]) extends ASTModuleMember

sealed abstract class ASTTypeClassMember extends ASTNode { def name: String }
case class ASTTypeClassMemberDefinition(name: String, qtype: ASTQType) extends ASTTypeClassMember
case class ASTTypeClassMemberImplementation(name: String, value: ASTExpr) extends ASTTypeClassMember

case class ASTTypeClassInstance(tcName: String,
                                context: List[ASTTypeClassReference],
                                params: List[ASTType],
                                members: List[ASTTypeClassMemberImplementation]) extends ASTModuleMember