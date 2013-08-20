package tap.types

import tap.types.Type._
import tap.{InstId, ModuleId, Id}
import tap.types.kinds.{Kfun, Star}
import language.reflectiveCalls


object Natives {

    val idArrow = ModuleId("Native", "->")
    val tArrow: Type = TCon(idArrow, Kfun(Star, Kfun(Star, Star)))

    val idNumber = ModuleId("Native", "Number")
    val tNumber: Type = TCon(idNumber, Star)

    val idString = ModuleId("Native", "String")
    val tString: Type = TCon(idString, Star)

    val idBool = ModuleId("Native", "Bool")
    val tBool: Type = TCon(idBool, Star)

    val idUnit = ModuleId("Native", "Unit")
    val tUnit: Type = TCon(idUnit, Star)

    val idVar = ModuleId("Native", "Var")
    val tVar = Type.quantify(List(TVar("a", Star)), TVar("a", Star) fn TAp(TCon(idVar, Kfun(Star, Star)), TVar("a", Star)))._2

    val `get!` = ModuleId("Native", "get!")
    val `set!` = ModuleId("Native", "set!")
    val `Num+Num` = InstId("Native", ModuleId("Native", "Plus"), List(idNumber), "+")
    val `Num-Num` = InstId("Native", ModuleId("Native", "Num"), List(idNumber), "-")
    val `Num/Num` = InstId("Native", ModuleId("Native", "Num"), List(idNumber), "/")
    val `Num*Num` = InstId("Native", ModuleId("Native", "Num"), List(idNumber), "*")
    val `Num%Num` = InstId("Native", ModuleId("Native", "Num"), List(idNumber), "mod")
    val `-Num` = InstId("Native", ModuleId("Native", "Num"), List(idNumber), "negate")
    val `String+String` = InstId("Native", ModuleId("Native", "Plus"), List(idString), "+")
    val `write-to-console` = ModuleId("Native", "write-to-console")
    val `Num==Num` = InstId("Data.Eq", ModuleId("Data.Eq", "Eq"), List(idNumber), "==")
    val `Num>Num` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(idNumber), ">")
    val `Num<Num` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(idNumber), "<")
    val `String==String` = InstId("Data.Eq", ModuleId("Data.Eq", "Eq"), List(idString), "==")
    val `String>String` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(idString), ">")
    val `String<String` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(idString), "<")
    val `showNum` = InstId("Text.Show", ModuleId("Text.Show", "Show"), List(idNumber), "show")
    val `readNum` = InstId("Text.Read", ModuleId("Text.Read", "Read"), List(idNumber), "read")

    val types: Map[Id, Type] = Map(

        `get!` -> (TAp(TCon(idVar, Kfun(Star, Star)), TVar("a", Star)) fn TVar("a", Star)),
        `set!` -> (TAp(TCon(idVar, Kfun(Star, Star)), TVar("a", Star)) fn (TVar("a", Star) fn TAp(TCon(idVar, Kfun(Star, Star)), TVar("a", Star)))),

        `Num+Num` -> (tNumber fn (tNumber fn tNumber)),
        `String+String` -> (tString fn (tString fn tString)),

        `Num-Num` -> (tNumber fn (tNumber fn tNumber)),
        `Num/Num` -> (tNumber fn (tNumber fn tNumber)),
        `Num*Num` -> (tNumber fn (tNumber fn tNumber)),
        `Num%Num` -> (tNumber fn (tNumber fn tNumber)),
        `-Num` -> (tNumber fn tNumber),

        `write-to-console` -> (tString fn tUnit),

        `Num==Num` -> (tNumber fn (tNumber fn tBool)),
        `String==String` -> (tString fn (tString fn tBool)),

        `Num>Num` -> (tNumber fn (tNumber fn tBool)),
        `Num<Num` -> (tNumber fn (tNumber fn tBool)),
        `String>String` -> (tString fn (tString fn tBool)),
        `String<String` -> (tString fn (tString fn tBool)),

        `showNum` -> (tNumber fn tString),
        `readNum` -> (tString fn tNumber)
    )

}
