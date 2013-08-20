package tap.verifier.defs

import tap.ir.TapExpr
import tap.types.Natives._
import tap.types._
import tap.types.classes.ClassEnvironments.Inst
import tap.types.classes.{Qual, TypeclassDef}
import tap.{ModuleId, Id}
import tap.types.kinds.{Star, Kfun}

case class ModuleDefinitions private (tcons: Map[ModuleId, TCon],
                             dcons: Map[ModuleId, Type],
                             tcs: Map[ModuleId, TypeclassDef],
                             tcis: Map[ModuleId, List[Inst]],
                             mts: Map[Id, Qual[Type]],
                             mis: Map[Id, TapExpr])

object ModuleDefinitions {
    val defaults = ModuleDefinitions(
        tcons = Map(
            ModuleId("Native", "->") -> tArrow.asInstanceOf[TCon],
            ModuleId("Native", "Number") -> tNumber.asInstanceOf[TCon],
            ModuleId("Native", "String") -> tString.asInstanceOf[TCon],
            ModuleId("Native", "Bool") -> tBool.asInstanceOf[TCon],
            ModuleId("Native", "Unit") -> tUnit.asInstanceOf[TCon],
            ModuleId("Native", "Var") -> TCon(ModuleId("Native", "Var"), Kfun(Star, Star))
        ),
        dcons = Map(
            ModuleId("Native", "True") -> tBool,
            ModuleId("Native", "False") -> tBool,
            ModuleId("Native", "Unit") -> tUnit,
            ModuleId("Native", "Var") -> tVar
        ),
        tcs = Map.empty,
        tcis = Map.empty,
        mts = Map.empty,
        mis = Map.empty)
}