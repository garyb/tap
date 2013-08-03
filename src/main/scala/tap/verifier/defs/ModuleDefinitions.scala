package tap.verifier.defs

import tap.ir.TapExpr
import tap.types.Type._
import tap.types._
import tap.types.classes.ClassEnvironments.Inst
import tap.types.classes.{Qual, TypeclassDef}
import tap.{ModuleId, Id}
import tap.types.kinds.{Star, Kfun}

case class ModuleDefinitions(tcons: Map[ModuleId, TCon],
                             dcons: Map[ModuleId, Type],
                             tcs: Map[ModuleId, TypeclassDef],
                             tcis: Map[ModuleId, List[Inst]],
                             mts: Map[Id, Qual[Type]],
                             mis: Map[Id, TapExpr])

object ModuleDefinitions {
    val empty = ModuleDefinitions(
        Map(
            ModuleId("Prelude", "->") -> tArrow.asInstanceOf[TCon],
            ModuleId("Prelude", "Var") -> TCon(ModuleId("Prelude", "Var"), Kfun(Star, Star))
        ),
        Map(
            ModuleId("Prelude", "Var") -> tVar
        ),
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty)
}