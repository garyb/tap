package tap.types.classes

import tap.ModuleId
import tap.ast.FilePositional
import tap.types.Tyvar

/**
 * @param name The name of the typeclass
 * @param ps The predicates (aka the typeclass context), used to determine superclasses
 * @param vs The type variables instances must provide types for.
 * @param members The members defined by the typeclass.
 * @param defaultMembers The members in the typeclass that also have default implementations.
 */
case class TypeclassDef(name: ModuleId, ps: List[IsIn], vs: List[Tyvar], members: Set[String], defaultMembers: Set[String]) extends FilePositional
