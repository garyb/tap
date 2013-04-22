package test.util

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.util.Graph

class GraphTests extends FlatSpec {

	behavior of "components"

	it should "identify strongly connected components" in {

		val edges1 = Map(
			"A" -> Set("B", "D"),
			"B" -> Set("A"),
			"C" -> Set("B"),
			"D" -> Set("A"),
			"E" -> Set("C", "F"),
			"F" -> Set("E")
		)

		Graph.components(edges1) should be ===
			List(List("D", "A", "B"), List("C"), List("F", "E"))

		val edges2 = Map(
			"A" -> Set("B"),
			"B" -> Set("A", "X"),
			"C" -> Set("A", "D"),
			"D" -> Set("C"),
			"X" -> Set("D")
		)

		Graph.components(edges2) should be ===
			List(List("B", "A", "C", "D", "X"))
	}

	it should "not require all nodes to be in the edges map" in {

		val edges = Map(
			"A" -> Set("B", "C"),
			"B" -> Set("A")
		)

		Graph.components(edges) should be ===
			List(List("C"), List("B", "A"))
	}

	it should "act like a topological sort when there are no components" in {

		val edges = Map(
			"A" -> Set("P"),
			"B" -> Set("Q"),
			"C" -> Set("R"),
			"Z" -> Set.empty[String],
			"P" -> Set("R"),
			"Q" -> Set("P")
		)

		Graph.components(edges) should be ===
			List(List("R"), List("P"), List("A"), List("Q"), List("B"), List("C"), List("Z"))
	}

	behavior of "makeComponentLookup"

	it should "create a lookup linking from each item back to its group" in {

		val edges = Map(
			"A" -> Set("B", "D"),
			"B" -> Set("A"),
			"C" -> Set("B"),
			"D" -> Set("A"),
			"E" -> Set("C", "F"),
			"F" -> Set("E")
		)

		val components = Graph.components(edges)
		val lookup = Graph.makeComponentLookup(components)

		lookup("A") should be === List("D", "A", "B")
		lookup("B") should be === List("D", "A", "B")
		lookup("C") should be === List("C")
		lookup("D") should be === List("D", "A", "B")
		lookup("E") should be === List("F", "E")
		lookup("F") should be === List("F", "E")
	}

	behavior of "tsort"

	it should "sort topologically" in {

		val edges = Map(
			"A" -> Set("P"),
			"B" -> Set("Q"),
			"C" -> Set("R"),
			"Z" -> Set.empty[String],
			"P" -> Set("R"),
			"Q" -> Set("P")
		)

		Graph.tsort(edges) should be ===
			List("R", "P", "A", "Q", "B", "C", "Z")
	}

	it should "throw an error if the graph has a cycle" in {

		val edges = Map(
			"A" -> Set("B", "C"),
			"B" -> Set("A")
		)

		evaluating { Graph.tsort(edges) } should produce [IllegalArgumentException]
	}

}
