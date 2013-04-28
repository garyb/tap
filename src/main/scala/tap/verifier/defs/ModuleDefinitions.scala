package tap.verifier.defs

import tap.ir.TapExpr
import tap.types.Type.tArrow
import tap.types._
import tap.types.classes.ClassEnvironments.Inst
import tap.types.classes.{Qual, TypeclassDef}
import tap.{ModuleId, Id}

case class ModuleDefinitions(tcons: Map[ModuleId, TCon],
                       dcons: Map[ModuleId, Type],
                       tcs: Map[ModuleId, TypeclassDef],
                       tcis: Map[ModuleId, List[Inst]],
                       mts: Map[Id, Qual[Type]],
                       mis: Map[Id, TapExpr])

object ModuleDefinitions {
    val empty = ModuleDefinitions(Map(ModuleId("Prelude", "->") -> tArrow.asInstanceOf[TCon]), Map.empty, Map.empty, Map.empty, Map.empty, Map.empty/*, Map.empty, Map.empty*/)
}