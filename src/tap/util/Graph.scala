package tap.util

object Graph {

	case class State[A](count: Int = 1,
	                    dfNumber: Map[A, Int] = Map.empty[A, Int],
	                    lowlinks: Map[A, Int] = Map.empty[A, Int],
	                    stack: List[A] = List.empty[A],
	                    result: List[List[A]] = List.empty[List[A]])

	def search[A](v: A, edges: Map[A, Iterable[A]], st: State[A]): State[A] = {

		val st1 = st.copy(
			dfNumber = st.dfNumber + (v -> st.count),
			lowlinks = st.lowlinks + (v -> st.count),
			count    = st.count + 1,
			stack    = v :: st.stack)

		val st2 = edges.get(v) match {
			case Some(es) =>
				es.foldLeft(st1) { (st, w) =>
					if (!(st.dfNumber contains w)) {
						val st1 = search(w, edges, st)
						val min = math.min(st1.lowlinks(v), st1.lowlinks(w))
						st1.copy(lowlinks = st1.lowlinks + (v -> min))
					} else if (st.stack contains w) {
						val min = math.min(st.lowlinks(v), st.dfNumber(w))
						st.copy(lowlinks = st.lowlinks + (v -> min))
					} else {
						st
					}
				}
			case None => st1
		}

		if (st2.lowlinks(v) != st.count) st2
		else {
			val (component, remain) = st2.stack.splitAt(st2.stack.indexOf(v) + 1)
			st2.copy(stack = remain, result = component :: st2.result)
		}
	}

	/**
	 * Extracts the set of vertices referenced within a map of edges.
	 */
	def extractVertices[A](edges: Map[A, Iterable[A]]): Set[A] =
		edges.foldLeft(Set.empty[A]) {
			case (vs, (a, as)) => vs ++ as + a
		}

	/**
	 * Finds and topologically sorts the strongly connected components in a graph.
	 */
	def components[A](vertices: Iterable[A], edges: Map[A, Iterable[A]]): List[List[A]] =
		vertices.foldLeft(State[A]()) { (st, v) =>
			if (st.dfNumber contains v) st
			else search(v, edges, st)
		}.result.reverse

	/**
	 * Finds and topologically sorts the strongly connected components in a graph, where the graph is defined by a set
	 * of edges and the vertices are extracted from the edges.
	 */
	def components[A](edges: Map[A, Iterable[A]]): List[List[A]] =
		components(extractVertices(edges), edges)

	/**
	 * Topologically sorts the vertices in an acyclic directed graph.
	 */
	def tsort[A](vertices: Iterable[A], edges: Map[A, Iterable[A]]): List[A] = {
		val out = components(vertices, edges)
		if (out forall { c => c.length == 1 }) out.flatten
		else throw new IllegalArgumentException("Cannot topologically sort non-acyclic graph")
	}

	/**
	 * Topologically sorts the vertices in an acyclic directed graph, where the graph is defined by a set of edges and
	 * the vertices are extracted from the edges.
	 */
	def tsort[A](edges: Map[A, Iterable[A]]): List[A] =
		tsort(extractVertices(edges), edges)

	/**
	 * Converts a list of edges (A -> B), (A -> C) into a map of the form (A -> (B, C, ...)).
	 */
	def edgesToMap[A](edges: Iterable[(A, A)]): Map[A, Iterable[A]] =
		edges groupBy { case (k, v) => k } mapValues { s => s map { case (k, v) => v } }
}
