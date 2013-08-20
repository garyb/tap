package tap.verifier.defs

import tap.ir.TapExpr
import tap.types._
import tap.types.classes.ClassEnvironments.Inst
import tap.types.classes.{Qual, TypeclassDef}
import tap.{ModuleId, Id}

case class ModuleDefinitions private (tcons: Map[ModuleId, TCon],
                             dcons: Map[ModuleId, Type],
                             tcs: Map[ModuleId, TypeclassDef],
                             tcis: Map[ModuleId, List[Inst]],
                             mts: Map[Id, Qual[Type]],
                             mis: Map[Id, TapExpr])

object ModuleDefinitions {
    val defaults = ModuleDefinitions(
        tcons = Natives.tcons,
        dcons = Natives.dcons,
        tcs = Map.empty,
        tcis = Map.empty,
        mts = Map.empty,
        mis = Map.empty)
}