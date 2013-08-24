package tap.types.kinds

import tap.Id
import tap.types._
import scala.annotation.tailrec

sealed trait Kind
case object Star extends Kind
case class Kfun(x: Kind, y: Kind) extends Kind
case class Kvar(q: Id, id: String) extends Kind

object Kind {

    /**
     * Finds the kind of a type.
     */
    def kind(t: Type): Kind = t match {
        case Forall(_, t) => kind(t)
        case TAp(t, _) => kind(t) match {
            case Kfun(_, k) => k
            case _ => throw new Error("kind * found on TAp type")
        }
        case TCon(_, k) => k
        case TVar(tv) => kind(tv)
        case MetaTv(m) => kind(m)
    }

    /**
     * Finds the kind of a type variable.
     */
    def kind(tv: TyVar): Kind = tv match {
        case BoundTv(_, k) => k
        case SkolemTv(_, _, k) => k
    }

    /**
     * Finds the kind of a meta type variable.
     */
    def kind(m: Meta): Kind = m match {
        case Meta(_, k, _) => k
    }

    @tailrec final def arity(k: Kind, depth: Int = 0): Int = k match {
        case Star => depth
        case Kfun(_, k) => arity(k, depth + 1)
        case _: Kvar => throw new Error("arity called on Kvar")
    }
}