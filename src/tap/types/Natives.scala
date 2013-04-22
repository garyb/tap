package tap.types

import tap.types.Type._
import tap.{InstId, ModuleId, Id}
import tap.types.kinds.{Kfun, Star}
import language.reflectiveCalls


object Natives {

	val `get!` = ModuleId("Prelude", "get!")
	val `set!` = ModuleId("Prelude", "set!")
	val `Num+Num` = InstId("Prelude", ModuleId("Prelude", "Plus"), List(getTConID(tNumber)), "+")
	val `Num-Num` = InstId("Prelude", ModuleId("Prelude", "Num"), List(getTConID(tNumber)), "-")
	val `Num/Num` = InstId("Prelude", ModuleId("Prelude", "Num"), List(getTConID(tNumber)), "/")
	val `Num*Num` = InstId("Prelude", ModuleId("Prelude", "Num"), List(getTConID(tNumber)), "*")
	val `Num%Num` = InstId("Prelude", ModuleId("Prelude", "Num"), List(getTConID(tNumber)), "mod")
	val `-Num` = InstId("Prelude", ModuleId("Prelude", "Num"), List(getTConID(tNumber)), "negate")
	val `String+String` = InstId("Prelude", ModuleId("Prelude", "Plus"), List(getTConID(tString)), "+")
	val `write-to-console` = ModuleId("Prelude", "write-to-console")
	val `Num==Num` = InstId("Data.Eq", ModuleId("Data.Eq", "Eq"), List(getTConID(tNumber)), "==")
	val `Num>Num` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(getTConID(tNumber)), ">")
	val `Num<Num` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(getTConID(tNumber)), "<")
	val `String==String` = InstId("Data.Eq", ModuleId("Data.Eq", "Eq"), List(getTConID(tString)), "==")
	val `String>String` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(getTConID(tString)), ">")
	val `String<String` = InstId("Data.Ord", ModuleId("Data.Ord", "Ord"), List(getTConID(tString)), "<")
	val `showNum` = InstId("Text.Show", ModuleId("Text.Show", "Show"), List(getTConID(tNumber)), "show")
	val `readNum` = InstId("Text.Read", ModuleId("Text.Read", "Read"), List(getTConID(tNumber)), "read")

	val types: Map[Id, Type] = Map(

		`get!` -> Type.quantify(List(Tyvar("a", Star)), (TAp(TCon(Tycon(ModuleId("Prelude", "Var"), Kfun(Star, Star))), TVar(Tyvar("a", Star))) fn TVar(Tyvar("a", Star)))),
		`set!` -> Type.quantify(List(Tyvar("a", Star)), (TAp(TCon(Tycon(ModuleId("Prelude", "Var"), Kfun(Star, Star))), TVar(Tyvar("a", Star))) fn (TVar(Tyvar("a", Star)) fn TVar(Tyvar("a", Star))))),

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
