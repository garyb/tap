package test.util

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import tap.util.Graph

class GraphTests extends FlatSpec {

	behavior of "components"

	it should "identify strongly connected components" in {

		val vertices = Set("A", "B", "C", "D", "E", "F")
		val edges = Map(
			"A" -> Set("B", "D"),
			"B" -> Set("A"),
			"C" -> Set("B"),
			"D" -> Set("A"),
			"E" -> Set("C", "F"),
			"F" -> Set("E")
		)

		Graph.components(vertices, edges) should be ===
			List(List("D", "A", "B"), List("C"), List("F", "E"))
	}

	it should "not require all nodes to be in the edges map" in {

		val vertices = Set("A", "B", "C")
		val edges = Map(
			"A" -> Set("B", "C"),
			"B" -> Set("A")
		)

		Graph.components(vertices, edges) should be ===
			List(List("C"), List("B", "A"))
	}

	it should "act like a topological sort when there are no components" in {

		val vertices = Set("Z", "A", "B", "C", "P", "Q", "R")
		val edges = Map(
			"A" -> Set("P"),
			"B" -> Set("Q"),
			"C" -> Set("R"),
			"P" -> Set("R"),
			"Q" -> Set("P")
		)

		Graph.components(vertices, edges) should be ===
			List(List("R"), List("P"), List("A"), List("Q"), List("B"), List("C"), List("Z"))
	}

	behavior of "tsort"

	it should "sort topologically" in {

		val vertices = Set("Z", "A", "B", "C", "P", "Q", "R")
		val edges = Map(
			"A" -> Set("P"),
			"B" -> Set("Q"),
			"C" -> Set("R"),
			"P" -> Set("R"),
			"Q" -> Set("P")
		)

		Graph.tsort(vertices, edges) should be ===
			List("R", "P", "A", "Q", "B", "C", "Z")
	}

	it should "throw an error if the graph has a cycle" in {
		val vertices = Set("A", "B", "C")
		val edges = Map(
			"A" -> Set("B", "C"),
			"B" -> Set("A")
		)

		evaluating { Graph.tsort(vertices, edges) } should produce [IllegalArgumentException]
	}

}
