package tap.types

import tap.ModuleId
import tap.types.kinds.{Kind, Kfun, Star}
import scala.annotation.tailrec
import tap.types.inference.Substitutions
import tap.util.PrettyPrint._
import language.implicitConversions
import language.reflectiveCalls

sealed trait Type
case class TVar(v: Tyvar) extends Type
case class TCon(c: Tycon) extends Type
case class TAp(f: Type, a: Type) extends Type
case class TGen(fi: Int, i: Int) extends Type
case class Forall(i: Int, ks: List[Kind], t: Type) extends Type

case class Tycon(id: ModuleId, k: Kind)
case class Tyvar(id: String, k: Kind)

object Type {

    private var forallId: Int = 0
    def newForallId(): Int = {
        forallId += 1
        forallId
    }
    def lastForallId: Int = forallId

    private def id(name: String) = ModuleId("Prelude", name)

    val tArrow: Type  = TCon(Tycon(id("->"), Kfun(Star, Kfun(Star, Star))))
    val tNumber: Type = TCon(Tycon(id("Number"), Star))
    val tString: Type = TCon(Tycon(id("String"), Star))
    val tBool: Type   = TCon(Tycon(id("Bool"), Star))
    val tUnit: Type   = TCon(Tycon(id("Unit"), Star))
    val tList: Type   = TCon(Tycon(id("List"), Kfun(Star, Star)))
    val tVar: Type    = TCon(Tycon(id("Var"), Star))
    val tAny: Type    = Type.quantify(List(Tyvar("a", Star)), TVar(Tyvar("a", Star)))

    /**
     * Constructs a function type.
     */
    implicit def toFn(a: Type) = new { def fn(b: Type): Type = TAp(TAp(tArrow, a), b) }

    /**
     * Finds the ID of a type constructor in the specified type. This should only be called when it is known the type
     * is a TCon or an application of a TCon.
     */
    def getTConID(t: Type): ModuleId = t match {
        case TCon(tc) => tc.id
        case TAp(t, _) => getTConID(t)
        case _ => throw new IllegalArgumentException("non-TCon or TAp in getTConID: " + t)
    }

    /**
     * Checks whether a type is a function type.
     */
    @tailrec def isFuncType(t: Type): Boolean = t match {
        case TAp(c: TCon, _) => c == tArrow
        case TAp(t, _) => isFuncType(t)
        case _ => false
    }

    /**
     * Extracts the argument and returns types from a function as a list..
     */
    def getFunctionTypes(t: Type): List[Type] = {
        def loop(t: Type): List[Type] = t match {
            case TAp(fn, arg) if fn == tArrow => List(arg)
            case TAp(fn, arg) if isFuncType(fn) => loop(fn) ++ loop(arg)
            case t => List(t)
        }
        loop(t)
    }

    /**
     * Extracts the argument types from a function type.
     */
    def getFunctionTypeArgs(t: Type): List[Type] = getFunctionTypes(t).dropRight(1)

    /**
     * Finds the arity of a function based on its type.
     */
    def getFunctionArity(t: Type): Int = getFunctionTypes(t).length - 1

    /**
     * Finds the arity of a type based on its kind.
     */
    def getTypeArity(t: Type): Int = Kind.arity(Kind.kind(t))

    /**
     * Creates a function type from a list of types. List(a, b, c) becomes (a -> b -> c).
     */
    def makeFunctionType(ts: List[Type]) = ts.reduceRight { _ fn _ }

    /**
     * Creates a function type from a list of argument types and a return type.
     */
    def makeFunctionType(argTs: List[Type], rt: Type) = argTs.foldRight(rt) { _ fn _ }

    /**
     * Universally quantifies t using the specified type variables.
     */
    def quantify(vs: List[Tyvar], t: Type): Type = {
        val vs1 = Substitutions.tv(t) collect { case v if vs contains v => v }
        if (vs1.isEmpty) t
        else {
            val ks = vs1 map Kind.kind
            val fi = newForallId()
            val s = (vs1 zip (List.range(0, vs1.size) map { n => TGen(fi, n) })).toMap
            Forall(fi, ks, Substitutions.applySubst(s, t))
        }
    }

    /**
     * Instantiates a universally quantified type, replacing the quantified types with other types.
     */
    def inst(sc: Forall, ts: List[Type], t: Type): Type = t match {
        case Forall(i, ks, t) if i == sc.i => inst(sc, ts, t)
        case Forall(i, ks, t) => Forall(i, ks, inst(sc, ts, t))
        case TAp(l, r) => TAp(inst(sc, ts, l), inst(sc, ts, r))
        case TGen(id, n) if id == sc.i => ts(n)
        case t => t
    }
}