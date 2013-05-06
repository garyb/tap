package tap.ast

import annotation.tailrec
import tap.{LocalId, ModuleId}
import tap.types.Type._
import tap.types._
import tap.verifier.errors._
import tap.types.kinds.{Kind, Kvar, KInfer}

/**
 * Useful operations on AST nodes.
 */
object ASTUtil {

    /**
     * Extracts the name of a type constructor from an ASTType.
     */
    @tailrec def getTConName(lookup: Map[String, ModuleId], ast: ASTType): ModuleId = ast match {
        case ASTTypeCon(id) => lookup(id)
        case ASTTypeApply(ttype, _) => getTConName(lookup, ttype)
        case _ => throw new Error("getTConName on non-ASTTypeApply or ASTTypeCon: " + ast)
    }

    /**
     * Finds all the type constructors used in an ASTType.
     */
    @tailrec def findTypeConstructors(tcons: Map[String, ModuleId], ast: ASTType, result: Set[ModuleId] = Set.empty): Set[ModuleId] = ast match {
        case ASTTypeCon(i) =>
            tcons.get(i) match {
                case Some(qi) => result + qi
                case None => throw UnknownTypeConstructorError(i, ast)
            }
        case ASTTypeVar(i) => result
        case ASTTypeApply(t, ps) => findAllTypeConstructors(tcons, t :: ps, result)
        case ASTFunctionType(ps) => findAllTypeConstructors(tcons, ps, result)
        case ASTForall(_, t) => findTypeConstructors(tcons, t, result)
    }

    /**
     * Finds all the type constructors used in a list of ASTTypes.
     */
    def findAllTypeConstructors(tcons: Map[String, ModuleId], asts: Iterable[ASTType], result: Set[ModuleId] = Set.empty): Set[ModuleId] =
        asts.foldLeft(result) { (result, ast) => findTypeConstructors(tcons, ast, result) }

    /**
     * Finds all the non-forall declared type variables used in an ASTType.
     */
    def findTypeVars(ast: ASTType, result: Set[String] = Set.empty): Set[String] = ast match {
        case ASTTypeCon(i) => result
        case ASTTypeVar(i) => result + i
        case ASTTypeApply(t, ps) => (t :: ps).foldRight(result)(findTypeVars)
        case ASTFunctionType(ps) => ps.foldRight(result)(findTypeVars)
        case ASTForall(ts, t) => result ++ (findTypeVars(t) filterNot { t => ts contains t })
    }

    /**
     * Converts an ASTType into a Type.
     */
    def getType(lookup: Map[String, ModuleId], tcons: Map[ModuleId, TCon], tvs: Map[String, TVar], ast: ASTType): Type = ast match {
        case ASTTypeCon(id) => tcons.getOrElse(lookup(id), throw UnknownTypeConstructorError(id, ast))
        case ASTTypeVar(id) => tvs.getOrElse(id, throw UnknownTypeVariableError(id, ast))
        case ASTTypeApply(ttype, params) =>
            val t = getType(lookup, tcons, tvs, ttype)
            val k = Kind.kind(t)
            val arity = Kind.arity(k)
            if (arity == 0) throw new TypeConstructorNoArgsError(t, ast)
            else if (params.length > arity) throw new TypeConstructorTooManyArgsError(t, ast)
            params.foldLeft(t) { (t,p) => TAp(t, getType(lookup, tcons, tvs, p)) }
        case ASTFunctionType(List(p)) => makeFunctionType(List(tUnit, getType(lookup, tcons, tvs, p)))
        case ASTFunctionType(params) => makeFunctionType(params map { ast => getType(lookup, tcons, tvs, ast) })
        case ASTForall(ids, t) =>
            if (ids.distinct != ids) throw new Error("forall contains duplicate type variable declaration")
            val msn = LocalId("forall")
            val ki = KInfer.constrain(lookup, tcons, msn, List(msn), List(t))
            val km = KInfer.solve(ki, ast)
            val ttvs = (ids map { p => p -> TVar(p, KInfer(km, Kvar(msn, p))) }).toMap
            val tt = getType(lookup, tcons, tvs ++ ttvs, t)
            quantify(ttvs.values.toList, tt)
    }

    /**
     * Checks whether a type is a concrete type or not. A concrete type is a type constructor or application of a type
     * constructor.
     */
    @tailrec final def isConcrete(t: ASTType): Boolean = t match {
        case ASTTypeApply(x, _) => isConcrete(x)
        case _: ASTTypeCon => true
        case _ => false
    }
}
