package tap.ast

import annotation.tailrec
import tap.{LocalId, ModuleId}
import tap.types.Type._
import tap.types._
import tap.verifier.errors._
import tap.types.kinds.{Kind, Kvar, KInfer}
import tap.types.classes.{IsIn, Qual}
import tap.types.inference.Substitutions.{nullSubst, Subst}
import tap.util.ContextOps.withContext

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
        case ASTForall(ts, t) =>
            ts find { t => result contains t } match {
                case Some(t) => throw TypeVariableOverlapError(t, ast)
                case None => result ++ ts ++ findTypeVars(t)
            }
    }

    /**
     * Converts an ASTType into a Type.
     */
    def getType(lookup: Map[String, ModuleId], tcons: Map[ModuleId, TCon], tvs: Map[String, TVar], ast: ASTType): Type =
        getTypeWithForallSubst(lookup, tcons, tvs, ast)._2

    def getTypeWithForallSubst(lookup: Map[String, ModuleId], tcons: Map[ModuleId, TCon], tvs: Map[String, TVar], ast: ASTType): (Subst, Type) = ast match {
        case ASTTypeCon(id) => (nullSubst, tcons.getOrElse(lookup(id), throw UnknownTypeConstructorError(id, ast)))
        case ASTTypeVar(id) => (nullSubst, tvs.getOrElse(id, throw UnknownTypeVariableError(id, ast)))
        case ASTTypeApply(ttype, params) =>
            val (s, t) = getTypeWithForallSubst(lookup, tcons, tvs, ttype)
            val k = Kind.kind(t)
            val arity = Kind.arity(k)
            if (arity == 0) throw new TypeConstructorNoArgsError(t, ast)
            else if (params.length > arity) throw new TypeConstructorTooManyArgsError(t, ast)
            params.foldLeft((s, t)) { case ((s0, t), p) =>
                val (s1, t1) = getTypeWithForallSubst(lookup, tcons, tvs, p)
                (s1, TAp(t, t1))
            }
        case ASTFunctionType(List(p)) =>
            val (s, t) = getTypeWithForallSubst(lookup, tcons, tvs, p)
            (s, makeFunctionType(List(tUnit, t)))
        case ASTFunctionType(params) =>
            val (s, ts) = withContext(nullSubst).map(params) { (s, ast) => getTypeWithForallSubst(lookup, tcons, tvs, ast) }
            (s, makeFunctionType(ts))
        case ASTForall(ids, t) =>
            if (ids.distinct != ids) throw new Error("forall contains duplicate type variable declaration")
            val msn = LocalId("forall")
            val ki = KInfer.constrain(lookup, tcons, msn, List(msn), List(t))
            val km = KInfer.solve(ki, ast)
            val ttvs = (ids map { p => p -> TVar(p, KInfer(km, Kvar(msn, p))) }).toMap
            val (s0, tt) = getTypeWithForallSubst(lookup, tcons, tvs ++ ttvs, t)
            val (s1, sc) = quantify(ttvs.values.toList, tt)
            (s0 ++ s1, sc)
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
