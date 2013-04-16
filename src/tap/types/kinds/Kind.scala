package tap.types.kinds

import tap.Id
import tap.types._

sealed trait Kind
case object Star extends Kind
case class Kfun(x: Kind, y: Kind) extends Kind
case class Kvar(q: Id, id: String) extends Kind

object Kind {

	/**
	 * Finds the kind of a type variable.
	 */
	def kind(t: Tyvar) = t.k

	/**
	 * Finds the kind of a type constant.
	 */
	def kind(t: Tycon) = t.k

	/**
	 * Finds the kind of a type.
	 */
	def kind(t: Type): Kind = t match {
		case TVar(u) => kind(u)
		case TCon(tc) => kind(tc)
		case Forall(_, _, t) => kind(t)
		case TAp(t, _) => kind(t) match {
			case Kfun(_, k) => k
			case _ => throw new Error("kind * found on TAp type")
		}
		case _: TGen => throw new Error("kind called on TGen")
	}
}