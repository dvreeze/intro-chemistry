/*
 * Copyright 2019-2019 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.introchemistry.internal

import eu.cdevreeze.introchemistry.internal.UndirectedGraph.Arc
import eu.cdevreeze.introchemistry.internal.UndirectedGraph.Edge
import eu.cdevreeze.introchemistry.internal.UndirectedGraph.Path

/**
 * Generic (non-empty) undirected graph. It can be used for checks on Lewis structures, when we consider multiple bonds
 * between the same pair of atom keys to be just one edge. Such an undirected graph can be used to check that the graph
 * is connected, and that it has no cycles.
 *
 * @author Chris de Vreeze
 */
final case class UndirectedGraph[A] private(vertices: Set[A], edges: Set[Edge[A]]) {
  require(vertices.nonEmpty, s"No vertices found")
  require(edges.toSeq.flatMap(_.endpoints).toSet.subsetOf(vertices), s"Not all edge endpoints are known vertices")

  /**
   * Functionally adds the given edges to the graph. Adding the same edges twice or more is a no-op, because the collection
   * of edges is a mathematical set.
   */
  def plusEdges(edgesToAdd: Set[Edge[A]]): UndirectedGraph[A] = {
    UndirectedGraph[A](vertices, this.edges.union(edgesToAdd))
  }

  /**
   * Returns true if this undirected graph is a tree. That is, returns true if it is a connected acyclic undirected graph.
   */
  def isTree: Boolean = isConnectedGraph && hasNoCycles(vertices.head)

  def isConnectedGraph: Boolean = {
    val edgesByVertices: Map[A, Seq[Edge[A]]] = computeEdgeMapping

    val firstVertex: A = vertices.head

    val connectedVerticesOrSelf: Set[A] = findAllConnectedVerticesOrSelf(firstVertex, Set.empty, edgesByVertices)

    connectedVerticesOrSelf == vertices
  }

  /**
   * Returns true if this graph has no cycles in the connected component containing the given vertex.
   */
  def hasNoCycles(vertex: A): Boolean = {
    val descendantPaths = findAllDescendantPaths(vertex)
    !descendantPaths.exists(_.containsCycle)
  }

  /**
   * Finds all descendant paths of the given vertex (depth-first), stopping recursion when encountering a path with duplicate vertices.
   * Those duplicates will be part of the function result, thus signalling that a cycle has been found.
   */
  def findAllDescendantPaths(vertex: A): Seq[Path[A]] = {
    val edgeMapping: Map[A, Seq[Edge[A]]] = computeEdgeMapping

    findAllDescendantPaths(vertex, edgeMapping)
  }

  private def computeEdgeMapping: Map[A, Seq[Edge[A]]] = {
    val edgesByFroms: Map[A, Seq[Edge[A]]] = edges.groupBy(_.firstEndpoint).view.mapValues(_.toSeq).toMap
    val edgesByTos: Map[A, Seq[Edge[A]]] = edges.groupBy(_.secondEndpoint).view.mapValues(_.toSeq).toMap

    vertices
      .map(v => v -> edgesByFroms.getOrElse(v, Seq.empty).appendedAll(edgesByTos.getOrElse(v, Seq.empty)).distinct)
      .toMap
  }

  /**
   * Adds to the visited set of vertices all vertices that are transitively connected to the given vertex,
   * or that are that given vertex itself.
   */
  private def findAllConnectedVerticesOrSelf(vertex: A, visited: Set[A], edgesByEndpoints: Map[A, Seq[Edge[A]]]): Set[A] = {
    val nonVisitedChildren: Set[A] =
      edgesByEndpoints.getOrElse(vertex, Seq.empty).flatMap(edge => edge.endpoints.filterNot(_ == vertex))
        .filterNot(visited).toSet.ensuring(!_.contains(vertex))

    if (nonVisitedChildren.isEmpty) {
      visited.union(Set(vertex))
    } else {
      val nextVisited: Set[A] = visited.union(nonVisitedChildren).union(Set(vertex))

      nonVisitedChildren.ensuring(!_.contains(vertex)).foldLeft(nextVisited) { case (accVisited, v) =>
        // Recursive call
        findAllConnectedVerticesOrSelf(v, accVisited, edgesByEndpoints)
      }
    }
  }

  /**
   * Finds all descendant paths of the given vertex (depth-first), stopping recursion when encountering a path with duplicate vertices.
   * Those duplicates will be part of the function result, thus signalling that a cycle has been found.
   */
  private def findAllDescendantPaths(vertex: A, edgesByEndpoints: Map[A, Seq[Edge[A]]]): Seq[Path[A]] = {
    val nextArcs: Seq[Arc[A]] = findOutgoingArcs(vertex, edgesByEndpoints)

    nextArcs.flatMap(arc => findAllDescendantPaths(Path.singletonPath(arc), edgesByEndpoints))
  }

  private def findAllDescendantPaths(startPath: Path[A], edgesByEndpoints: Map[A, Seq[Edge[A]]]): Seq[Path[A]] = {
    val lastArc = startPath.endArc

    // Stop growing the path if it has already a cycle
    val nextArcs: Seq[Arc[A]] =
      if (startPath.containsCycle) {
        Seq.empty
      } else {
        findOutgoingArcs(lastArc.to, edgesByEndpoints).filterNot(_.to == lastArc.from) // This filter is very important!
      }

    val nextPaths: Seq[Path[A]] = nextArcs.map(arc => startPath.append(arc))

    if (nextPaths.isEmpty) {
      Seq(startPath)
    } else {
      nextPaths.flatMap { nextPath =>
        // Recursive calls
        findAllDescendantPaths(nextPath, edgesByEndpoints)
      }
    }
  }

  private def findOutgoingArcs(vertex: A, edgesByEndpoints: Map[A, Seq[Edge[A]]]): Seq[Arc[A]] = {
    val toVertices = edgesByEndpoints.getOrElse(vertex, Seq.empty).flatMap(edge => edge.endpoints.filterNot(_ == vertex)).distinct
    toVertices.map(to => Arc(vertex, to))
  }
}

object UndirectedGraph {

  /**
   * Edge, which is conceptually a set of 2 distinct vertices. The edge class does hold a "first" and "second" vertex for
   * convenience, but equality of edges is indeed set equality.
   *
   * Implementation note: if Scala 2.13 had a Set variant of SeqMap, that could have been used instead for the endpoints set,
   * and then class Edge would have been a case class.
   */
  final class Edge[A] private(val endpoints: Seq[A]) {
    require(endpoints.sizeIs == 2 && endpoints.distinct.sizeIs == 2, s"An edge must have exactly 2 distinct endpoints")

    def firstEndpoint: A = endpoints.head

    def secondEndpoint: A = endpoints.tail.head

    override def equals(other: Any): Boolean = other match {
      case other: Edge[_] => endpoints.toSet == other.endpoints.toSet
      case _ => false
    }

    override def hashCode: Int = endpoints.toSet.hashCode

    override def toString: String = s"Edge($firstEndpoint, $secondEndpoint)"
  }

  object Edge {

    def apply[A](endpoint1: A, endpoint2: A): Edge[A] = new Edge(Seq(endpoint1, endpoint2))
  }

  def fromVertices[A](vertices: Set[A]): UndirectedGraph[A] = {
    UndirectedGraph(vertices, Set.empty)
  }

  /**
   * Directed edge, or arc.
   */
  final case class Arc[A](from: A, to: A) {
    require(from != to, s"An arc cannot point from a vertex to the same vertex")

    def endpoints: Seq[A] = Seq(from, to)
  }

  final case class Path[A](arcs: Seq[Arc[A]]) {
    require(arcs.nonEmpty, s"Empty path not allowed")
    require(
      arcs.sliding(2).filter(_.sizeIs == 2).forall(pair => pair(0).to == pair(1).from),
      s"Not a path of subsequent arcs")

    def startArc: Arc[A] = arcs.head

    def endArc: Arc[A] = arcs.last

    def allVertices: Seq[A] = arcs.flatMap(_.endpoints).distinct

    def containsCycle: Boolean = {
      val vertices = allVertices
      vertices.sizeIs < arcs.size + 1
    }

    def append(arc: Arc[A]): Path[A] = Path[A](arcs.appended(arc))

    def prepend(arc: Arc[A]): Path[A] = Path[A](arcs.prepended(arc))
  }

  object Path {

    def singletonPath[A](arc: Arc[A]): Path[A] = Path(Seq(arc))
  }
}
