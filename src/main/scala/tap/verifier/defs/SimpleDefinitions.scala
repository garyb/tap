package tap.verifier.defs

import tap.ModuleId
import tap.ir.{TapNode, TapExpr}
import tap.types.{TCon, Type}

case class SimpleDefinitions(tcons: Map[ModuleId, TCon],
                             dcons: Map[ModuleId, Type],
                             mis: Map[ModuleId, TapExpr],
                             ets: Map[TapNode, Type])
