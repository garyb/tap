package tap.types.classes

import tap.ModuleId
import tap.types._
import tap.types.classes.ClassEnvironments._
import tap.types.inference.Substitutions._
import tap.types.inference.Unify
import scala.annotation.tailrec

case class IsIn(id: ModuleId, ts: List[Type])

object IsIn {

	def inst(sc: Forall, ts: List[Type], p: IsIn): IsIn =
		IsIn(p.id, p.ts map { p => Type.inst(sc, ts, p) })

	def mguPred(t1: IsIn, t2: IsIn): Option[Subst] =
		(t1, t2) match {
			case (IsIn(i1, ts1), IsIn(i2, ts2)) if i1 == i2 => Unify.mgus(ts1, ts2)
			case _ => None
		}

	def matchPred(t1: IsIn, t2: IsIn): Option[Subst] =
		(t1, t2) match {
			case (IsIn(i1, ts1), IsIn(i2, ts2)) if i1 == i2 => Unify.matches(ts1, ts2)
			case _ => None
		}

	// -----------------------------------------------------------------------------------------------------------
	//  Entailment
	// -----------------------------------------------------------------------------------------------------------

	/**
	 * Gets a list of predicates that must hold true for p based on superclass information.
	 */
	def bySuper(ce: ClassEnv, p: IsIn): List[IsIn] = {
		val s = (sig(ce, p.id) zip p.ts).toMap
		val supers = `super`(ce, p.id) map { p => applySubst(s, p) }
		p :: (supers flatMap { case p => bySuper(ce, p) })
	}

	/**
	 * Gets a list of predicates that must hold true for p based on the instance for p in the class environment.
	 */
	def byInst(ce: ClassEnv, p: IsIn): Option[List[IsIn]] = {
		val ss = insts(ce, p.id).view map { q =>
			matchPred(q.tc, p) flatMap { u =>
				Some(q.ps map { p => applySubst(u, p) })
			}
		}
		ss.foldRight(None: Option[List[IsIn]]) {
			case (None, y) => y
			case (x, _) => x
		}
	}

	/**
	 * Checks whether p can be deduced from ps using superclasses.
	 */
	def scEntail(ce: ClassEnv, ps: List[IsIn], p: IsIn): Boolean =
		ps flatMap { p => bySuper(ce, p) } contains p

	/**
	 * Checks whether p can be deduced from ps using superclasses and existing instances.
	 */
	def entail(ce: ClassEnv, ps: List[IsIn], p: IsIn): Boolean =
		scEntail(ce, ps, p) || (byInst(ce, p) match {
			case Some(qs) => qs forall { q => entail(ce, ps, q) }
			case None => false
		})

	// -----------------------------------------------------------------------------------------------------------
	//  Context reduction
	// -----------------------------------------------------------------------------------------------------------

	/**
	 * Removes predicates that always hold true.
	 */
	def elimTauts(ce: ClassEnv, ps: List[IsIn]): List[IsIn] =
		ps filterNot { p => entail(ce, Nil, p) }

	/**
	 * Reduces a list of predicates by removing duplicates, and predicates that are implied by superclass information.
	 */
	def reduce(ce: ClassEnv, ps: List[IsIn]): List[IsIn] = {
		@tailrec def loop(rs: List[IsIn], ps: List[IsIn]): List[IsIn] = ps match {
			case (p :: ps) if scEntail(ce, elimTauts(ce, rs ++ ps), p) => loop(rs, ps)
			case (p :: ps) => loop(p :: rs, ps)
			case List() => rs
		}
		loop(Nil, ps)
	}

	/**
	 * Splits a list of predicates into deferred and retained predicates. The deferred predicates are passed out as
	 * constraints to the enclosing scope, the retained predicates are used to form the current type.
	 */
	def split(ce: ClassEnv, vs: List[Tyvar], ps: List[IsIn]): (List[IsIn], List[IsIn]) =
		reduce(ce, ps) partition { p => tv(p) forall { tv => vs contains tv } }
}