import scala.collection.mutable.{HashMap, HashSet}

/**
  * Created by sorinmanole on 14/07/2016.
  */

/**
  * Class that implements Dijkstra algorithm for a weighted directed graph
  *
  * @param  graph  graph to execute Dijkstra algorithm on
  */
class DijkstraAlgorithm(val graph: Graph) {
  val nodes: List[Vertex] = graph.vertexes
  val edges: List[Edge] = graph.edges
  val distances: HashMap[Vertex, Double] = new HashMap[Vertex, Double]()
  private val evaluatedNodes: HashSet[Vertex] = new HashSet[Vertex]()
  private val unevaluatedNodes: HashSet[Vertex] = new HashSet[Vertex]()
  private val antecessors: HashMap[Vertex, Vertex] = new HashMap[Vertex, Vertex]()

  /**
    * Executes the dijkstra algorithm for a source node/vertex
    *
    * @param  source  the source vertex
    */
  def executeAlgorithm(source: Vertex): Unit = {
    distances.put(source, 0)
    unevaluatedNodes.add(source)

    while (unevaluatedNodes.nonEmpty) {
      val node: Vertex = getMinNode(unevaluatedNodes)
      evaluatedNodes.add(node)
      unevaluatedNodes.remove(node)
      calculateMinDistances(node)
    }
  }

  /**
    * Gets the minimum from a set of vertexes
    *
    * @param  vertexes  the set of vertexes
    */
  def getMinNode(vertexes: HashSet[Vertex]): Vertex = {
    var minimum: Vertex = null

    for (vertex <- vertexes) {
      if (minimum == null) {
        minimum = vertex
      } else {
        if (getMinDistance(vertex) < getMinDistance(minimum)){
          minimum = vertex
        }
      }
    }

    minimum
  }

  /**
    * Gets the shortest distance between the source node and a target node
    *
    * @param  target  the set of vertexes
    */
  def getMinDistance(target: Vertex): Double = {
    distances.getOrElse(target, Double.MaxValue)
  }

  /**
    * Finds the minimal distances from the input node to the rest of the nodes
    *
    * @param  node  the input node
    */
  def calculateMinDistances(node: Vertex): Unit ={
    val adjacentNodes = getNodeNeighbours(node)

    for (target <- adjacentNodes){
      if (getMinDistance(target) > getMinDistance(node) + getNodesDistance(node, target)){
        distances.put(target, getMinDistance(node) + getNodesDistance(node, target))
        antecessors.put(target, node)
        unevaluatedNodes.add(target)
      }
    }
  }

  /**
    *  Gets the neighbour nodes for an input node
    *
    * @param  node  the input node
    */
  def getNodeNeighbours(node: Vertex): List[Vertex] ={
    var neighbours: List[Vertex] = Nil

    for (edge <- edges){
      if (edge.source.equals(node) && !isEvaluated(edge.destination)){
        neighbours = edge.destination :: neighbours
      }
    }

    neighbours
  }

  /**
    * Checks if a node has been yet evaluated
    * i.e. the shortest path between source and itself has been found
    *
    * @param  node  the input node
    */
  def isEvaluated(node: Vertex): Boolean ={
    evaluatedNodes.contains(node)
  }

  /**
    * Gets the distance between the source node and a target node
    *
    * @param  node  the source node
    * @param  target  the target node
    */
  def getNodesDistance(node: Vertex, target: Vertex): Double ={
    var distance: Double = 0
    for (edge <- edges){
      if (edge.source.equals(node) && edge.destination.equals(target)){
        distance = edge.weight
      }
    }

    distance
  }

  /**
    * Gets the shortest path between the source node and a target node
    *
    * @param  target  the target node
    */
  def getShortestPath(target: Vertex): List[Vertex] ={
    var path: List[Vertex] = Nil
    var step = target

    if (antecessors.get(step).isEmpty){
      return Nil
    }

    path = step :: path

    while(antecessors.get(step).isDefined) {
      step = antecessors.getOrElse(step, null)
      path = step :: path
    }

    path
  }
}
