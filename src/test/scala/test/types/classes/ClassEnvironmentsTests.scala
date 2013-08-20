package test.types.classes

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.ModuleId
import tap.types._
import tap.types.Natives._
import tap.types.classes.ClassEnvironments._
import tap.types.classes.{IsIn, TypeclassDef}
import tap.types.inference.TIError
import tap.types.kinds._
import test.TypeFixture

class ClassEnvironmentsTests extends FlatSpec with TypeFixture {

    val idX = ModuleId("Test", "X")
    val idY = ModuleId("Test", "Y")

    //-------------------------------------------------------------------------

    behavior of "addClass"

    it should "return a modified environment containing the class" in {
        val ps = List.empty
        val vs = List(TVar("a", Star))
        val ce = addClass(nullEnv, TypeclassDef(idX, ps, vs, Set.empty, Set.empty))
        ce should be === Map(idX -> (vs, ps, List.empty))
    }

    it should "throw an error if the class has no variables" in {
        val id = ModuleId("Test", "TC")
        evaluating {
            addClass(nullEnv, TypeclassDef(id, List.empty, List.empty, Set.empty, Set.empty))
        } should produce [Error]
    }

    it should "throw an error if a class with the same name already exists" in {
        val ce = addClass(nullEnv, TypeclassDef(idX, List.empty, List(TVar("a", Star)), Set.empty, Set.empty))
        evaluating {
            addClass(ce, TypeclassDef(idX, List.empty, List(TVar("b", Star)), Set.empty, Set.empty))
        } should produce [TIError]
    }

    it should "throw an error if a superclass is not already in the environment" in {
        val id1 = idX
        val id2 = idY
        evaluating {
            addClass(nullEnv, TypeclassDef(id2, List(IsIn(id1, List(TVar("b", Star)))), List(TVar("b", Star)), Set.empty, Set.empty))
        } should produce [Error]
    }

    it should "throw an error if superclass context contains non-variable types" in {
        val id1 = idX
        val id2 = idY
        val ce = addClass(nullEnv, TypeclassDef(id1, List.empty, List(TVar("a", Star)), Set.empty, Set.empty))
        evaluating {
            addClass(ce, TypeclassDef(id2, List(IsIn(id1, List(tString))), List(TVar("b", Star)), Set.empty, Set.empty))
        } should produce [Error]
    }

    it should "add a class with a superclass to the environment if the superclass is already present" in {
        val id1 = idX
        val id2 = idY
        val ce = addClass(nullEnv, TypeclassDef(id1, List.empty, List(TVar("a", Star)), Set.empty, Set.empty))
        addClass(ce, TypeclassDef(id2, List(IsIn(id1, List(TVar("b", Star)))), List(TVar("b", Star)), Set.empty, Set.empty))
    }

    //-------------------------------------------------------------------------

    behavior of "addInst"

    it should "return a modified environment containing the instance" in {
        val id = ModuleId("Test", "TC")
        val ps = List.empty
        val vs = List(TVar("a", Star))
        val ce1 = addClass(nullEnv, TypeclassDef(id, ps, vs, Set.empty, Set.empty))
        val inst = Inst("Test", List.empty, IsIn(id, List(tString)))
        val ce2 = addInst(ce1, inst)
        ce2 should be === Map(id -> (vs, ps, List(inst)))
    }

    it should "throw an error if the new instance is overlapping an existing instance" in {
        val id = ModuleId("Test", "TC")
        val ps = List.empty
        val vs = List(TVar("a", Star))
        val ce1 = addClass(nullEnv, TypeclassDef(id, ps, vs, Set.empty, Set.empty))
        val inst = Inst("Test", List.empty, IsIn(id, List(tString)))
        val ce2 = addInst(ce1, inst)
        evaluating { addInst(ce2, inst) } should produce [Error]
    }

    //-------------------------------------------------------------------------

    "checkInsts" should "throw an error if an instance has context that depends on instances that do not exist" in {

        /**
         * (class X (x) ...
         * (class Y (=> (X y)) (y) ...
         * (instance X (=> (X a) (X b)) ('(a b)) ...
         * (instance Y (=> (Y p)) ('(p q)) ...
         * The instance for Y is invalid because (X q) is not guaranteed, whereas (X p) is guaranteed via the (Y p)
         * constraint, as X is a superclass of Y
         */

        val tvA = TVar("a", Star)
        val tvB = TVar("b", Star)
        val tvP = TVar("p", Star)
        val tvQ = TVar("q", Star)
        val tvX = TVar("x", Star)
        val tvY = TVar("y", Star)

        val tcX = TypeclassDef(idX, List.empty, List(tvX), Set.empty, Set.empty)
        val tcY = TypeclassDef(idY, List(IsIn(idX, List(tvY))), List(tvY), Set.empty, Set.empty)

        val instX = Inst("Test", List(IsIn(idX, List(tvA)), IsIn(idX, List(tvB))), IsIn(idX, List(tTuple2(tvA, tvB))))
        val instY = Inst("Test", List(IsIn(idY, List(tvP))), IsIn(idY, List(tTuple2(tvP, tvQ))))

        val ce1 = addClass(addClass(nullEnv, tcX), tcY)
        val ce2 = addInst(ce1, instX)
        val ce3 = addInst(ce2, instY)
        evaluating { checkInsts(ce3) } should produce [Error]
    }

    //-------------------------------------------------------------------------

    behavior of "sig"

    it should "return the type variable signature of the specified class" in {
        val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test","X"), List.empty, List(TVar("a", Star)), Set.empty, Set.empty))
        val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test","Y"), List.empty, List(TVar("b", Star), TVar("c", Star)), Set.empty, Set.empty))
        sig(ce2, ModuleId("Test" ,"X")) should be === List(TVar("a", Star))
        sig(ce2, ModuleId("Test" ,"Y")) should be === List(TVar("b", Star), TVar("c", Star))
    }

    //-------------------------------------------------------------------------

    behavior of "super"

    it should "return the type variable signature of the specified class" in {
        val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test","X"), List.empty, List(TVar("a", Star)), Set.empty, Set.empty))
        val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test","Y"), List(IsIn(idX, List(TVar("c", Star)))), List(TVar("b", Star), TVar("c", Star)), Set.empty, Set.empty))
        `super`(ce2, ModuleId("Test" ,"X")) should be === List.empty
        `super`(ce2, ModuleId("Test" ,"Y")) should be === List(IsIn(idX, List(TVar("c", Star))))
    }

    //-------------------------------------------------------------------------

    behavior of "insts"

    it should "return the instances of the specified class" in {
        val ce1 = addClass(nullEnv, TypeclassDef(ModuleId("Test","X"), List.empty, List(TVar("a", Star)), Set.empty, Set.empty))
        val ce2 = addClass(ce1, TypeclassDef(ModuleId("Test","Y"), List.empty, List(TVar("b", Star)), Set.empty, Set.empty))
        val ce3 = addInst(ce2, Inst("Test", List.empty, IsIn(idY, List(tString))))
        insts(ce3, ModuleId("Test" ,"X")) should be === List.empty
        insts(ce3, ModuleId("Test" ,"Y")) should be === List(Inst("Test", List.empty, IsIn(idY, List(tString))))
    }
 }
