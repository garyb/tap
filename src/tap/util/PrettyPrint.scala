package tap.util

import annotation.tailrec
import tap.ir._
import tap.types.Type._
import tap.types._
import tap.types.kinds._
import tap.types.classes.{TypeclassDef, Qual, IsIn}
import tap.types.inference.Substitutions
import tap.{LocalId, InstId, ModuleId, Id}
import tap.types.classes.ClassEnvironments.Inst

object PrettyPrint {

	def prettyPrint(node: TapNode): String = node match {
		// TODO: list prettyprinting
		// TODO: line breaking/indenting for begin and matches
		case BlockExpr(es) => "(begin " + (es map prettyPrint mkString " ") + ")"
		//case ValueReadExpr(ModuleScoped(ModuleId("Prelude"), "EOL")) => "[]"
		case ApplyExpr(f, e) =>
			def uncurry(f: TapExpr, args: List[TapExpr]): (TapExpr, List[TapExpr]) = f match {
				case ApplyExpr(f, e) => uncurry(f, e :: args)
				case _ => (f, args)
			}
			val (fn, args) = uncurry(f, List(e))
			"(" + prettyPrint(fn) + " " + (args map prettyPrint mkString " ") + ")"
		case MatchExpr(e, cs) => "(match " + prettyPrint(e) + " " + (cs map prettyPrint mkString " ") + ")"
		case LetExpr(id, e, ValueReadExpr(LocalId(id1))) if id == id1 => "(let " + id + " " + prettyPrint(e) + ")"
		case LetExpr(id, e, f) => "(let " + id + " " + prettyPrint(e) + ") " + prettyPrint(f)
		case ValueReadExpr(id) => prettyPrint(id)
		case StringExpr(v) => "\"" + v + "\""
		case NumberExpr(v) => if ((v % 1) == 0) v.toString.dropRight(2) else v.toString
		case FunctionExpr(a, e) =>
			def uncurry(e: TapExpr, args: List[ArgumentDef]): (TapExpr, List[ArgumentDef]) = e match {
				case FunctionExpr(a, e) => uncurry(e, a :: args)
				case _ => (e, args)
			}
			val (fn, args) = uncurry(e, List(a))
			fn match {
				case BlockExpr(es) => "(lambda (" + (args.reverse map prettyPrint mkString " ") + ") " + (es map prettyPrint mkString " ") + ")"
				case fn => "(lambda (" + (args.reverse map prettyPrint mkString " ") + ") " + prettyPrint(fn) + ")"
			}

		case NativeValueExpr(id, _) => "native"
		case CastExpr(e, t) => "(cast " + prettyPrint(e) + " " + prettyPrint(t) + ")"
		case RaiseErrorExpr(e) => "(error " + prettyPrint(e) + ")"
		case Argument(id) => id
		case MatchCase(v, None, e) => "(case " + prettyPrint(v) + " " + prettyPrint(e) + ")"
		case MatchCase(v, Some(g), e) => "(case-if " + prettyPrint(v) + " " + prettyPrint(g) + " " + prettyPrint(e) + ")"
		case UnapplyNode(dcon, Seq()) => prettyPrint(dcon)
		case UnapplyNode(dcon, as) => "(" + prettyPrint(dcon) + " " + (as map prettyPrint mkString " ") + ")"
		case BindNode(id, None) => id
		case BindNode(id, Some(v)) => "[" + id + " " + prettyPrint(v) + "]"
		case NoArgument | WildcardValueExpr => "_"
	}

	@tailrec private def isList(e: TapExpr): Boolean = e match {
		case ApplyExpr(f, _) => isList(f)
		case ValueReadExpr(ModuleId("Prelude", ":")) => true
		case _ => false
	}

	def prettyPrint(ms: Id): String = ms match {
		case ModuleId(mId, id) => mId + "." + id
		case InstId(mId, tc, params, id) => mId + "[" + prettyPrint(tc) + "(" + (params map prettyPrint mkString ", ") + ")]." + id
		case LocalId(id) => id
	}

	def prettyPrint(k: Kind): String = {
		def loop(k: Kind, grp: Boolean): String = k match {
			case Star => "*"
			case Kvar(q, id) => prettyPrint(q) + "." + id
			case Kfun(x, y) if grp => "(" + loop(x, true) + " -> " + loop(y, false) + ")"
			case Kfun(x, y) => loop(x, true) + " -> " + loop(y, false)
		}
		loop(k, false)
	}

	def prettyPrint(argMods: List[List[IsIn]]): String = argMods map { ps => ps map { p => prettyPrint(p) }} toString()

	def prettyPrint(p: IsIn): String = prettyPrint(p.id) + " (" + p.ts.map { p => prettyPrint(p) }.mkString(", ") + ")"

	def prettyPrint(tc: TypeclassDef): String = {
		val TypeclassDef(name, context, params, _, _) = tc
		if (context.isEmpty) "class " + prettyPrint(name) + " (" + params.map(prettyPrint).mkString(", ") + ")"
		else "class (" + (context map prettyPrint mkString ", ") + ") => " + prettyPrint(name) + " (" + params.map(prettyPrint).mkString(", ") + ")"
	}

	def prettyPrint(tv: Tyvar): String = tv.id

	def prettyPrint(tci: Inst): String = {
		val Inst(mId, context, tc) = tci
		if (context.nonEmpty) "instance " + prettyPrint(tc.id) + " (" + (context map prettyPrint mkString ", ") + ") => " + (tc.ts map prettyPrint mkString ", ")
		else "instance " + prettyPrint(tc) + " " + (tc.ts map prettyPrint mkString ", ")
	}

	def prettyPrint(qt: Qual[Type]): String = {
		if (qt.ps.nonEmpty) "(" + (qt.ps map { p => prettyPrint(p) }).mkString(", ") + ") => " + prettyPrint(qt.h)
		else prettyPrint(qt.h)
	}

	/**
	 * Takes a type and returns a pretty-printed string representation for it.
	 */
	def prettyPrint(t: Type): String = {

		@tailrec @inline def isNonFuncApp(t: Type): Boolean = t match {
			case TAp(c: TCon, _) => c != tArrow
			case TAp(t1, _) => isNonFuncApp(t1)
			case _ => true
		}

		def loop(t: Type, grp: Boolean, top: Boolean = false): String = t match {
			case TVar(Tyvar(name, _)) => name
			case TCon(Tycon(name, _)) => prettyPrint(name)
				// TODO: list prettyprinting
			//case TAp(t1, t2) if t1 == tList => "[" + loop(t2, false) + "]"
			case TAp(t1, t2) if t1 == tArrow => loop(t2, true)
			case t @ TAp(t1, t2) if isFuncType(t1) =>
				if (grp) "(" + loop(t1, true) + " -> " + loop(t2, isNonFuncApp(t2)) + ")"
				else loop(t1, true) + " -> " + loop(t2, isNonFuncApp(t2))

			case TAp(t1, t2) if grp || top => "(" + loop(t1, false) + " " + loop(t2, true) + ")"

			case TAp(t1, t2) => loop(t1, false) + " " + loop(t2, true)
			case TGen(fi, i) => "§" + fi + "." + i
			case Forall(fi, ks, t) => "(forall " + ((0 until ks.length) map { i => "§" + fi + "." + i } mkString " ") + " " + loop(t, true) + ")"
		}
		loop(t, false, true)
	}
}
