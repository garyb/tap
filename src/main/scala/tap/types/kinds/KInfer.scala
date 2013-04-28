package tap.types.kinds

import Kind._
import annotation.tailrec
import tap.Id
import tap.ModuleId
import tap.ast._
import tap.types.TCon
import tap.verifier.errors.{KindConflictError, KindMismatchError, UnknownTypeConstructorError}
import scala.Some

object KInfer {

    type TypeConstructors = Map[ModuleId, TCon]

    var genID = -1
    def newKVar(q: Id): Kind = {
        genID += 1
        Kvar(q, "µ" + genID)
    }

    def apply(kis: Map[Kvar, Kind], ki: Kind): Kind = ki match {
        case Star => Star
        case v: Kvar => Star
            kis.get(v) match {
                case None => Star
                case Some(ki) => apply(kis, ki)
            }
        case Kfun(x, y) => Kfun(apply(kis, x), apply(kis, y))
    }

    /**
     * Finds constraints imposed on kinds based on usage within a list of types.
     * @param tcs The type constructors currently in scope.
     * @param current The name of the current thing being resolved (probably a type constructor or typeclass).
     * @param allCurrent The names of the current group of things being resolved.
     * @param types The list of types to use to infer kinds from.
     */
    def constrain(lookup: Map[String, ModuleId], tcs: TypeConstructors,
                  current: Id, allCurrent: Seq[Id],
                  types: Seq[ASTType]): Seq[(Kind, Kind)] = {

        def assume(t: ASTType): (Kind, List[(Kind, Kind)]) = t match {

            case ASTFunctionType(ps) =>
                val eq2 = ps.foldRight(List.empty[(Kind,  Kind)]) {
                    (p, eq0) =>
                        val (pk, eq1) = assume(p)
                        // Function arguments are values so add constraint that they have kind *
                        pk match {
                            case kv: Kvar => (kv, Star) :: eq0 ++ eq1
                            case _ => eq0 ++ eq1
                        }
                }
                (Star, eq2)

            case ASTTypeVar(id) => (Kvar(current, id), List.empty)

            case ASTTypeCon(id) => tcs.get(lookup(id)) match {
                case Some(tc) => (kind(tc), List.empty)
                case None if allCurrent contains lookup(id) => (newKVar(current), List.empty)
                case None => throw UnknownTypeConstructorError(id, t)
            }

            case ASTTypeApply(f, ps) =>
                val (fk, eq1) = assume(f)
                val rk = newKVar(current)
                val (ik, eq2) = ps.foldRight((rk, List.empty[(Kind,  Kind)])) {
                    case (p, (k, eq0)) =>
                        val (pk, eq1) = assume(p)
                        (Kfun(pk, k), eq0 ++ eq1)
                }
                (rk, (fk, ik) :: eq1 ++ eq2)

            case ASTForall(_, t) => assume(t)
        }
        types.flatMap { a =>
            val (ak, eq) = assume(a)
            // Ensure kind * is inferred at a minimum
            (Star, ak) :: eq
        }
    }

    /**
     * Solves a list of kind constraints to produce a map specifiying a kind for each KVar or KGen
     */
    def solve(eq: Seq[(Kind, Kind)], src: FilePositional): Map[Kvar, Kind] = {

        def applySubst(sbst: Map[Kvar, Kind], k: Kind): Kind = k match {
            case k: Kvar if sbst contains k => applySubst(sbst, sbst(k))
            case Kfun(x, y) => Kfun(applySubst(sbst, x), applySubst(sbst, y))
            case k => k
        }

        def containsParam(t: Kind, k: Kind): Boolean = t match {
            case Kfun(x, y) => containsParam(x, k) || containsParam(y, k)
            case t => t == k
        }

        @inline def solveParam(p: Kvar, t: Kind, eq: List[(Kind, Kind)], sbst: Map[Kvar, Kind]) = {
            val s = Map(p -> t)
            val s1 = eq collect { case (p, v) => (applySubst(s, p), applySubst(s, v)) }
            val s2 = (sbst collect { case (n, u) => n -> applySubst(s, u) }) + (p -> t)
            solve(s1, s2)
        }

        @tailrec def solve(eq: Seq[(Kind, Kind)], sbst: Map[Kvar, Kind]): Map[Kvar, Kind] = eq match {
            case (t1, t2) :: eq if t1 == t2 => solve(eq, sbst)
            case (k1: Kvar, k2: Kvar) :: eq => solveParam(k2, k1, eq, sbst)
            case (k: Kvar, t) :: eq if !containsParam(t, k) => solveParam(k, t, eq, sbst)
            case (t, k: Kvar) :: eq if !containsParam(t, k) => solveParam(k, t, eq, sbst)
            case (Kfun(x1, y1), Kfun(x2, y2)) :: eq => solve((x1, x2) :: (y1, y2) :: eq, sbst)
            case (t1, t2) :: _ => throw KindConflictError(t1, t2, src)
            case Seq() => sbst
        }

        solve(eq, Map.empty)
    }

}