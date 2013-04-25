package tap.ast

/**
 * Representation of exports from a module.
 */
sealed trait Export

/**
 * A member export.
 */
case class ExMember(name: String) extends Export

/**
 * A data type export, specifiying which data constructors are exported.
 */
case class ExDataType(name: String, dcons: Set[String]) extends Export

/**
 * A typeclass export.
 */
case class ExClass(name: String) extends Export

/**
 * A module export - all of the exports of the named module will be exported alongside the current module's exports.
 */
case class ExModule(name: String) extends Export