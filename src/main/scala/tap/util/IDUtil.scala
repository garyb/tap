package tap.util

import annotation.tailrec

object IDUtil {

    def makeId(id: String, reserved: Set[String]): String = {
        if (reserved contains id) tryNewId(id, reserved, 1)
        else id
    }

    def makeAlphabeticId(n: Int, reserved: Set[String]): String = {
        val aId = (97 + (n % 26)).toChar.toString
        makeId(aId, reserved)
    }

    @tailrec def tryNewId(id: String, reserved: Set[String], n: Int): String = {
        val newId = id + n
        if (reserved contains newId) tryNewId(id, reserved, n + 1)
        else newId
    }
}
