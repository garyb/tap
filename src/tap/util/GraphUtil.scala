package tap.util

import collection.immutable.Stack
import scala.math._
import annotation.tailrec

/**
 * Created by IntelliJ IDEA.
 * User: matthew.lloyd
 * Date: 09/09/11
 * Time: 10:32
 * To change this template use File | Settings | File Templates.
 */

object GraphUtil {

    object EdgeClass extends Enumeration {
        type EdgeClass = Value
        val Tree, Back, Forward, Cross = Value
    }

    case class DFSState[T, T2](data: T2,
                     time: Int = 1,
                     discovered: Set[T] = Set.empty[T],
                     parentMapping: Map[T, T] = Map.empty[T, T],
                     processed: Set[T] = Set.empty[T],
                     entryTime: Map[T, Int] = Map.empty[T, Int],
                     exitTime: Map[T, Int] = Map.empty[T, Int]) {

        def initState(node: T): DFSState[T, T2] =
            DFSState(data,
                     time + 1,
                     discovered + node,
                     parentMapping,
                     processed,
                     entryTime + (node -> (time + 1)),
                     exitTime)

        def setParent(node: T, parent: T): DFSState[T, T2] =
            DFSState(data,
                     time,
                     discovered,
                     parentMapping + (node -> parent),
                     processed,
                     entryTime,
                     exitTime)

        def finalizeState(node: T): DFSState[T, T2] =
            DFSState(data,
                     time + 1,
                     discovered,
                     parentMapping,
                     processed + node,
                     entryTime,
                     exitTime + (node -> (time + 1)))

        def updateData(newData:T2):DFSState[T, T2] =
            DFSState(newData,
                     time,
                     discovered,
                     parentMapping,
                     processed,
                     entryTime,
                     exitTime)
    }

    def classifyEdge[T, T2](x: T, y: T, state: DFSState[T, T2]) = {
        import EdgeClass._

        (x, y) match {
            case _ if (state.parentMapping.contains(y) && state.parentMapping(y) == x) => Tree
            case _ if (state.discovered.contains(y) && !state.processed.contains(y)) => Back
            case _ if (state.processed.contains(y) && (state.entryTime(y) > state.entryTime(x))) => Forward
            case _ if (state.processed.contains(y) && (state.entryTime(y) < state.entryTime(x))) => Cross
            case _ => throw GraphError("unclassified edge ("+x+","+y+")")
        }
    }

	/**
	 * Gets the topographical order of a set of strong connected components. Only works for directed graphs. Results
	 * are returned as a list rather than a stack. Cons lists are already LIFO.
	 * @param edges A map of the ir edges.
	 * @param nonDependent Graph nodes that may be referenced in an edge but have no dependencies of their own.
	 */
	def getSCCOrder[T](edges: Map[T, Seq[T]], nonDependent: Set[T]) = {
		// TODO: poor implementation needs revising
		// Stupid thing 1: requiring all nodes to be present in edges map
		// Stupid thing 2 that can't be avoided until #1 is fixed: result contains non-dependent nodes
		val (scv, sce, fun) = GraphUtil.getStronglyConnectedComponents(edges.keys ++ nonDependent, edges ++ nonDependent.map { _ -> Seq.empty })
		(stackToList(GraphUtil.getTopographicalOrder(scv, sce, true).filter { s => nonDependent.forall { nd => !(s contains nd) } }),
			fun.mapValues { vs => vs filterNot { v => nonDependent contains v } })
	}

	/**
	 * Converts a stack into a cons list, with the head of the stack being the first item in the list.
	 */
	def stackToList[T](xs: collection.immutable.Stack[T]): List[T] = xs.foldLeft(List.empty[T]) { (result, item) => item :: result }

    def getStronglyConnectedComponents[T](verticies: Iterable[T], edges: Map[T, Seq[T]], isDirected: Boolean = true): (Seq[Seq[T]], Map[Seq[T], Seq[Seq[T]]], Map[T, Seq[T]]) = {

        def popSSC(node: T, state: DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])]):DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])] = {
            val (stack, output, lowMapping) = state.data
            val (newStack, seq, _) = stack.foldLeft((stack, Seq.empty[T], true)) {(tup, lastValue) =>
                val (stack, seq, canGo) = tup

                if(canGo)
                    (stack.pop, seq :+ lastValue, lastValue != node)
                else tup
            }
            val newOutput = seq.foldLeft(output) { (output, n) =>
                output + (n -> seq)
            }
            state.updateData((newStack, newOutput, lowMapping))
        }

        def processEarly(node: T, state: DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])]): DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])] = {
            val (stack, output, lowMapping) = state.data
            state.updateData((stack.push(node), output, lowMapping))
        }

        def processEdge(node: T, targetNode: T, state: DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])]): DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])] = {
            import EdgeClass.{Back, Cross}

            val (stack, output, lowMapping) = state.data
            val newLow = (classifyEdge(node, targetNode, state) match {
                case Back if(state.entryTime(targetNode) < state.entryTime(lowMapping(node))) =>
                    lowMapping + (node -> targetNode)
                case Cross if(!output.contains(targetNode) && state.entryTime(targetNode) < state.entryTime(lowMapping(node))) =>
                    lowMapping + (node -> targetNode)
                case _ => lowMapping
            })

            state.updateData((stack, output, newLow))
        }

        def processLate(node: T, state: DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])]): DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])] = {
            val (_, _, lowMapping) = state.data
            val newState = if(lowMapping(node) == node) popSSC(node, state) else state

            val newLow = if(newState.parentMapping.contains(node) && newState.entryTime(lowMapping(node)) < newState.entryTime(lowMapping(newState.parentMapping(node))))
                     lowMapping + (newState.parentMapping(node) -> lowMapping(node))
                else lowMapping

            newState.updateData((newState.data._1, newState.data._2, newLow))
        }

        val initLow = verticies.foldLeft(Map.empty[T, T]) { (mapOut, node) =>
            mapOut + (node -> node)
        }

       	val nodeToSeq = (verticies.foldLeft(DFSState[T, (Stack[T], Map[T, Seq[T]], Map[T, T])]((Stack.empty[T], Map.empty[T, Seq[T]], initLow): (Stack[T], Map[T, Seq[T]], Map[T, T]))) { (state, node) =>
            if(!state.discovered.contains(node))
                dfs(node, edges, isDirected, state, processEarly, processEdge, processLate)
            else
                state
       	}).data._2

		val sscNodes = nodeToSeq.foldLeft(Seq.empty[Seq[T]]) { (out, tup) =>
           tup match {
               case (k, v) if(!out.contains(v)) => out :+ v
               case _ => out
           }
       	}

	    val sscMap2 = nodeToSeq.values.foldLeft(Map.empty[Seq[T], Seq[Seq[T]]]) { case (result, grp) =>

		    if (result contains grp) result
		    else {
			    val links = grp.foldLeft(Seq.empty: Seq[Seq[T]]) { case (result, entry) =>
				    result ++ ((edges(entry) map { ee => nodeToSeq(ee) filterNot { e => grp contains e } }) filter { _.nonEmpty })
			    }
			    result + (grp -> links)
		    }
	    }

		(sscNodes, sscMap2, nodeToSeq)
    }

    def getTopographicalOrder[T](verticies: Seq[T], edges: Map[T, Seq[T]], isDirected: Boolean = true): Stack[T] = {

        type topState = DFSState[T, Stack[T]]

        def processEarly(node: T, state: topState): topState = {
            state
        }

        def processEdge(node: T, targetNode: T, state: topState): topState = {
            import EdgeClass.Back
            val clazz = classifyEdge(node, targetNode, state)
            if (clazz == Back) throw GraphError("Graph is not acyclic")
            state
        }

        def processLate(node: T, state: topState): topState = {
            state.updateData(state.data.push(node))
        }

       (verticies.foldLeft(DFSState[T, Stack[T]](Stack.empty[T])) { (state, node) =>
            if(!state.discovered.contains(node))
                dfs(node, edges, isDirected, state, processEarly, processEdge, processLate)
           else state
        }).data
    }

    def dfs[T, T2](node:T, edges: Map[T, Seq[T]], isDirected: Boolean, initData: DFSState[T, T2],
               processEarly: (T, DFSState[T, T2]) => DFSState[T, T2],
               processEdge: (T, T, DFSState[T, T2]) => DFSState[T, T2],
               processLate: (T, DFSState[T, T2]) => DFSState[T, T2]): DFSState[T, T2] = {

        def innerDfs(node: T, state: DFSState[T, T2]): DFSState[T, T2] = {
            processLate(node, edges.getOrElse(node, Seq.empty).foldLeft(processEarly(node, state.initState(node))) { (state, targetNode) =>
                    targetNode match {
                        case _ if (!state.discovered.contains(targetNode)) =>
                            innerDfs(targetNode, processEdge(node, targetNode, state.setParent(targetNode, node)))
                        case _ if (isDirected || !state.processed.contains(targetNode)) =>
                            processEdge(node, targetNode, state)
                        case _ => state
                    }
            }).finalizeState(node)
        }

        innerDfs(node, initData)
    }
}

case class GraphError(msg: String) extends Exception(msg)