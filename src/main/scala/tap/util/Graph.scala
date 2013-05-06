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
     * Finds and topologically sorts the strongly connected components in a graph.
     */
    def components[A](edges: Map[A, Iterable[A]]): List[List[A]] =
        edges.keys.foldLeft(State[A]()) { (st, v) =>
            if (st.dfNumber contains v) st
            else search(v, edges, st)
        }.result.reverse

    /**
     * Creates a lookup for finding the component a vertex belongs to from a list of components.
     */
    def makeComponentLookup[A](components: List[List[A]]): Map[A, List[A]] =
        components.foldLeft(Map[A, List[A]]()) { (result, comp) =>
            comp.foldLeft(result) { (result, v) => result + (v -> comp) }
        }

    /**
     * Topologically sorts the vertices in an acyclic directed graph.
     * TODO: proper implementation - result is correct, but inefficient
     */
    def tsort[A](edges: Map[A, Iterable[A]]): List[A] = {
        val out = components(edges)
        if (out forall { c => c.length == 1 }) out.flatten
        else throw new IllegalArgumentException("Cannot topologically sort non-acyclic graph")
    }

    /**
     * Finds all the successors of an item in a graph.
     */
    def successors[A](item: A, edges: Map[A, Iterable[A]]): Set[A] = {
        def find(item: A, seen: Set[A] = Set.empty): Set[A] =
            edges.get(item) match {
                case Some(items) =>
                    items.foldLeft(seen + item) { case (seen, item) =>
                        if (seen contains item) seen
                        else find(item, seen + item)
                    }
                case None => seen + item
            }
        find(item) - item
    }
}