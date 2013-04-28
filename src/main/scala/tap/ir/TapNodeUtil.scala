package tap.ir

import annotation.tailrec
import tap.{LocalId, Id}


object TapNodeUtil {

    /**
     * Gets the function value at the bottom of a chain of applications.
     */
    def getAppliedFunc(ap: ApplyExpr): TapExpr = {
        @tailrec def find(expr: TapExpr): TapExpr = expr match {
            case ApplyExpr(f, e) => find(f)
            case e => e
        }
        find(ap)
    }

    /**
     * Extracts the list of arguments being applied in a chain of applications.
     */
    @tailrec def getApplyArgs(ap: ApplyExpr, args: List[TapExpr] = List.empty): List[TapExpr] = ap match {
        case ApplyExpr(fn: ApplyExpr, arg) => getApplyArgs(fn, arg :: args)
        case ApplyExpr(fn, arg) => arg :: args
    }

    /**
     * Creates an apply expression, applying each item in xs to fn.
     */
    @tailrec def makeApply(fn: TapExpr, xs: Iterable[TapExpr]): TapExpr = xs match {
        case x :: xs => makeApply(ApplyExpr(fn, x), xs)
        case _ => fn
    }

    /**
     * Creates an function value, accepting arguments xs before evaluating body.
     */
    def makeFunc(xs: Iterable[ArgumentDef], body: TapExpr): TapExpr = xs match {
        case x :: xs => FunctionExpr(x, makeFunc(xs, body))
        case _ => body
    }

    /**
     * Construct a block expression around xs if needed. If xs is empty default is returned.
     * TODO: might be better to remove the default argument and error on an empty list instead
     */
    def makeBlock(xs: List[TapExpr], default: TapExpr) = xs match {
        case List() => default
        case List(expr) => expr
        case xs => BlockExpr(xs)
    }

    /**
     * Finds all references to other members or type constructors within an expr.
     */
    def findDependencies(expr: TapNode, result: Set[Id] = Set.empty): Set[Id] = expr match {
        case _: ArgumentDef => result
        case UnapplyNode(_: LocalId, xs) => result
        case UnapplyNode(ref, xs) => xs.foldLeft(result + ref) { (result, x) => findDependencies(x, result) }
        case BindNode(_, Some(cn)) => findDependencies(cn, result)
        case WildcardValueExpr => result
        case MatchCase(cn, None, e) => findDependencies(cn, findDependencies(e, result))
        case MatchCase(cn, Some(g), e) =>findDependencies(cn, findDependencies(g, findDependencies(e, result)))
        case ValueReadExpr(_: LocalId) => result
        case ValueReadExpr(ref) => result + ref
        case BlockExpr(es) => es.foldLeft(result) { (result, e) => findDependencies(e, result) }
        case ApplyExpr(f, e) => findDependencies(f, findDependencies(e, result))
        case MatchExpr(e, cs) => findDependencies(e, cs.foldLeft(result) { (result, cn) => findDependencies(cn, result) })
        case LetExpr(_, e, es) => findDependencies(e, findDependencies(es, result))
        case FunctionExpr(a, e) => findDependencies(e, result)
        case CastExpr(e, _) => findDependencies(e, result)
        case RaiseErrorExpr(e) => findDependencies(e, result)
        case _: ExprLeaf => result
        case _ => result
    }

    /**
     * Finds all references to other members or type constructors within an expr that will be required for the
     * expression to evaluate. Refs inside a lambda do not count, as the lambda isn't being evaluated right now.
     * TODO: this isn't very sophisticated in its current form... but great care needs to be taken to ensure this
     * function always returns dependencies, even if some are false positives. it is used in a check that rejects
     * programs that have members that are initialised in a mutually recursive manner, as these will cause the runtime
     * to hang while the modules are still initializing, before any program code has actually executed.
     */
    def findImmediateDependencies(expr: TapExpr, result: Set[Id] = Set.empty): Set[Id] = expr match {
        case FunctionExpr(a, e) => Set.empty
        case expr => findDependencies(expr)
    }

    /**
     * Finds all declared and referenced local ids under node.
     */
    def findLocalIds(node: TapNode, result: Set[String] = Set.empty): Set[String] = node match {
        case Argument(id) => result + id
        case NoArgument => result
        case BindNode(id, Some(e)) => findLocalIds(e, result + id)
        case BindNode(id, None) => result + id
        case UnapplyNode(d, as) => as.foldLeft(result) { (result, e) => findLocalIds(e, result) }
        case WildcardValueExpr => result
        case MatchCase(p, Some(g), e) => findLocalIds(p, findLocalIds(g, findLocalIds(e, result)))
        case MatchCase(p, None, e) => findLocalIds(p, findLocalIds(e, result))
        case ValueReadExpr(LocalId(id)) => result + id
        case ValueReadExpr(_) => result
        case LetExpr(id, e, es) => findLocalIds(e, result + id) ++ findLocalIds(es, result)
        case BlockExpr(es) => es.foldLeft(result) { (result, e) => findLocalIds(e, result) }
        case ApplyExpr(f, e) => findLocalIds(f, findLocalIds(e, result))
        case MatchExpr(e, cs) => cs.foldLeft(findLocalIds(e, result)) { (result, e) => findLocalIds(e, result) }
        case FunctionExpr(a, e) => findLocalIds(a, findLocalIds(e, result))
        case RaiseErrorExpr(e) => findLocalIds(e, result)
        case CastExpr(e, _) => findLocalIds(e, result)
        case _: ExprLeaf => result
    }
}
