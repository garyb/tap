package test

import tap.ModuleId
import tap.types.Type._
import tap.types._
import tap.types.kinds._

trait TypeFixture {

    def tTuple2(a: Type, b: Type): Type =
        TAp(TAp(TCon(ModuleId("Prelude", "Tuple2"), Kfun(Star, Kfun(Star, Star))), a), b)

    val tList: Type = TCon(ModuleId("Prelude", "List"), Kfun(Star, Star))

}
