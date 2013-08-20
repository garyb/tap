package tap.types

import tap.types.Type._
import tap.ModuleId
import tap.types.kinds.{Kfun, Star}
import language.reflectiveCalls

object Natives {

    val idArrow = ModuleId("Native", "->")
    val tArrow = TCon(idArrow, Kfun(Star, Kfun(Star, Star)))

    val idNumber = ModuleId("Native", "Number")
    val tNumber = TCon(idNumber, Star)

    val idString = ModuleId("Native", "String")
    val tString = TCon(idString, Star)

    val idBool = ModuleId("Native", "Bool")
    val idTrue = ModuleId("Native", "True")
    val idFalse = ModuleId("Native", "False")
    val tBool = TCon(idBool, Star)

    val idUnit = ModuleId("Native", "Unit")
    val tUnit = TCon(idUnit, Star)

    val idVar = ModuleId("Native", "Var")
    val tVar = TCon(idVar, Kfun(Star, Star))

    val `get!` = ModuleId("Native", "get!")
    val `set!` = ModuleId("Native", "set!")

    val `Num+Num`  = ModuleId("Native", "addNumNum")
    val `Num-Num`  = ModuleId("Native", "subNumNum")
    val `Num/Num`  = ModuleId("Native", "divNumNum")
    val `Num*Num`  = ModuleId("Native", "mulNumNum")
    val `Num%Num`  = ModuleId("Native", "modNumNum")
    val `-Num`     = ModuleId("Native", "negateNum")
    val `Num==Num` = ModuleId("Native", "eqNumNum")
    val `Num>Num`  = ModuleId("Native", "gtNumNum")
    val `Num<Num`  = ModuleId("Native", "ltNumNum")
    val `showNum`  = ModuleId("Native", "showNum")
    val `readNum`  = ModuleId("Native", "readNum")

    val `String+String`  = ModuleId("Native", "addStringString")
    val `String==String` = ModuleId("Native", "eqStringString")
    val `String>String`  = ModuleId("Native", "gtStringString")
    val `String<String`  = ModuleId("Native", "ltStringString")

    val `write-to-console` = ModuleId("Native", "write-to-console")

    val tcons: Map[ModuleId, TCon] = Map(
        idArrow -> tArrow,
        idNumber -> tNumber,
        idString -> tString,
        idBool -> tBool,
        idUnit -> tUnit,
        idVar -> tVar
    )

    val types: Map[ModuleId, Type] = Map(

        idTrue -> tBool,
        idFalse -> tBool,

        idVar -> Type.quantify(List(TVar("a", Star)), TVar("a", Star) fn TAp(TCon(idVar, Kfun(Star, Star)), TVar("a", Star)))._2,

        idUnit -> tUnit,

        `get!` -> (TAp(TCon(idVar, Kfun(Star, Star)), TVar("a", Star)) fn TVar("a", Star)),
        `set!` -> (TAp(TCon(idVar, Kfun(Star, Star)), TVar("a", Star)) fn (TVar("a", Star) fn TAp(TCon(idVar, Kfun(Star, Star)), TVar("a", Star)))),

        `Num+Num`  -> (tNumber fn (tNumber fn tNumber)),
        `Num-Num`  -> (tNumber fn (tNumber fn tNumber)),
        `Num/Num`  -> (tNumber fn (tNumber fn tNumber)),
        `Num*Num`  -> (tNumber fn (tNumber fn tNumber)),
        `Num%Num`  -> (tNumber fn (tNumber fn tNumber)),
        `-Num`     -> (tNumber fn tNumber),
        `Num==Num` -> (tNumber fn (tNumber fn tBool)),
        `Num>Num`  -> (tNumber fn (tNumber fn tBool)),
        `Num<Num`  -> (tNumber fn (tNumber fn tBool)),
        `showNum`  -> (tNumber fn tString),
        `readNum`  -> (tString fn tNumber),

        `String+String`  -> (tString fn (tString fn tString)),
        `String==String` -> (tString fn (tString fn tBool)),
        `String>String`  -> (tString fn (tString fn tBool)),
        `String<String`  -> (tString fn (tString fn tBool)),

        `write-to-console` -> (tString fn tUnit)
    )

    val dcons = types filter { id => Seq(idTrue, idFalse, idUnit, idVar) contains id }

}
