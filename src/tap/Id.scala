package tap

/**
 * An ID for a type constructor, data constructor, typeclass, member, variable, or constant.
 */
sealed trait Id { def id: String }

/**
 * An ID for something defined within a module; a type constructor, data constructor, member or typeclass.
 */
case class ModuleId(mId: String, id: String) extends Id

/**
 * An ID for a member belonging to a typeclass instance.
 */
case class InstId(mId: String, tc: ModuleId, params: List[ModuleId], id: String) extends Id

/**
 * An ID for a local variable or constant.
 */
case class LocalId(id: String) extends Id