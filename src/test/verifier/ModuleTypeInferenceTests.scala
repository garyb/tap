package test.verifier

import org.scalatest.{GivenWhenThen, FlatSpec}
import org.scalatest.matchers.ShouldMatchers._

class ModuleTypeInferenceTests extends FlatSpec {

	// TODO: a binding group test (this was failing):
	//
	// (def tmpX (-> a a))
	// (let tmpX (lambda (x) (tmpD x) x))
	//
	// (let tmpA (lambda (a) (tmpB a) a))
	// (let tmpB (lambda (b) (tmpA b) (tmpX b) b))
	//
	// (let tmpC (lambda (c) (tmpA c) (tmpD c) c))
	// (let tmpD (lambda (d) (tmpC d) d))
	//
	// (as a dot:
	// digraph {
	//	    X [shape=box]
	//	    A -> B
	//	    B -> A
	//	    B -> X
	//	    C -> A
	//	    C -> D
	//	    D -> C
	//	    X -> D
	//	  }
	// )
	//
	// desired BG: { [tmpX], ([tmpA, tmpB], [tmpC, tmpD]) }

}
