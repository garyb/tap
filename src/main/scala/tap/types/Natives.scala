package tap.types

import tap.types.Type._
import tap.{InstId, ModuleId, Id}
import tap.types.kinds.{Kfun, Star}
import language.reflectiveCalls


object Natives {

    val tArrow: Type  = TCon(ModuleId("Native", "->"), Kfun(Star, Kfun(Star, Star)))
    val tNumber: Type = TCon(ModuleId("Native", "Number"), Star)
    val tString: Type = TCon(ModuleId("Native", "String"), Star)
    val tBool: Type   = TCon(ModuleId("Native", "Bool"), Star)
    val tUnit: Type   = TCon(ModuleId("Native", "Unit"), Star)
    val tVar: Type    = Type.quantify(List(TVar("a", Star)), TVar("a", Star) fn TAp(TCon(ModuleId("Native", "Var"), Kfun(Star, Star)), TVar("a", Star)))._2

    val `get!` = ModuleId("Native", "get!")
    val `set!` = ModuleId("Native", "set!")
    val `Num+Num` = InstId("Native", ModuleId("Native", "Plus"), List(getTConID(tNumber)), "+")
    val `Num-Num` = InstId("Native", ModuleId("Native", "Num"), List(getTConID(tNumber)), "-")
    val `Num/Num` = InstId("Native", ModuleId("Native", "Num"), List(getTConID(tNumber)), "/")
    val `Num*Num` = InstId("Native", ModuleId("Native", "Num"), List(getTConID(tNumber)), "*")
    val `Num%Num` = InstId("Native", ModuleId("Native", "Num"), List(getTConID(tNumber)), "mod")
    val `-Num` = InstId("Native", ModuleId("Native", "Num"), List(getTConID(tNumber)), "negate")
    val `String+String` = InstId("Native", ModuleId("Native", "Plus"), List(getTConID(tString)), "+")
    val `write-to-console` = ModuleId("Native", "write-to-console")
    val `Num==Num` = InstId("Data.Eq", ModuleId("Data.Eq", "Eq"), List(getTConID(tNumber)), "==")
    val `Num>Num` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(getTConID(tNumber)), ">")
    val `Num<Num` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(getTConID(tNumber)), "<")
    val `String==String` = InstId("Data.Eq", ModuleId("Data.Eq", "Eq"), List(getTConID(tString)), "==")
    val `String>String` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(getTConID(tString)), ">")
    val `String<String` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(getTConID(tString)), "<")
    val `showNum` = InstId("Text.Show", ModuleId("Text.Show", "Show"), List(getTConID(tNumber)), "show")
    val `readNum` = InstId("Text.Read", ModuleId("Text.Read", "Read"), List(getTConID(tNumber)), "read")

    val types: Map[Id, Type] = Map(

        `get!` -> (TAp(TCon(ModuleId("Native", "Var"), Kfun(Star, Star)), TVar("a", Star)) fn TVar("a", Star)),
        `set!` -> (TAp(TCon(ModuleId("Native", "Var"), Kfun(Star, Star)), TVar("a", Star)) fn (TVar("a", Star) fn TAp(TCon(ModuleId("Native", "Var"), Kfun(Star, Star)), TVar("a", Star)))),

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
