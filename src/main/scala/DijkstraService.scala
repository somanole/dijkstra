/**
  * Created by sorinmanole on 14/07/2016.
  */

/**
  * Object that implements Dijkstra the I/O methods needed Dijkstra Algorithm
  *
  */
object DijkstraService {
  var nodes: List[Vertex] = Nil
  var edges: List[Edge] = Nil
  var startStation = ""
  var endStation = ""
  var nearbyStation = ""
  var nearbyDistance: Double = 0

  def addEdge(edgeId: String, start: String, end: String, distance: Double): Unit ={
    edges = new Edge(edgeId, nodes.find(x => x.id == start).get, nodes.find(x => x.id == end).get, distance) :: edges
  }

  /**
    * Read graph from StdIn
    *
    */
  def readGraph(): Unit ={
    val edgesInput = scala.io.StdIn.readLine()

    for (i <- 0 until edgesInput.toInt){
      val route = scala.io.StdIn.readLine()
      val destinations = route.split(":")(0)
      val distance = route.split(":")(1).trim

      val start = destinations.split("->")(0).trim
      val end = destinations.split("->")(1).trim
      val vertexStart = new Vertex(start)
      val vertexEnd = new Vertex(end)

      if (!nodes.contains(vertexStart)){
        nodes = vertexStart :: nodes
      }

      if (!nodes.contains(vertexEnd)){
        nodes = vertexEnd :: nodes
      }

      addEdge("Edge_" + i, start, end, distance.toDouble)
    }
  }

  /**
    * Read route to calculate shortest path for from StdIn
    *
    */
  def readRouteToCalculatePath(): Unit={
    val route = scala.io.StdIn.readLine()

    startStation = route.split("->")(0).trim.split(" ")(1).trim
    endStation = route.split("->")(1).trim
  }

  /**
    * Read station to calculate nearby stations within a range for from StdIn
    *
    */
  def readStationToCalculateNearby(): Unit ={
    val nearbyInput = scala.io.StdIn.readLine()

    nearbyStation = nearbyInput.split(",")(0).trim.split(" ")(1).trim
    nearbyDistance = nearbyInput.split(",")(1).trim.toDouble
  }

  /**
    * Calculate shortest path between source and target read from StdIn
    *
    */
  def calculateShortestPath(): String ={
    val graph = new Graph(nodes, edges)
    val dijkstra = new DijkstraAlgorithm(graph)

    //calculate shortest path between start and end stations
    dijkstra.executeAlgorithm(nodes.find(x => x.id == startStation).get)
    val path = dijkstra.getShortestPath(nodes.find(x => x.id == endStation).get)

    //build message for printing
    var message = ""
    if (path.isEmpty){
      message = "Error: No route from " + startStation + " to " + endStation
    } else {
      var totalDistance: Double = 0
      var previousVertex = path.head

      for (vertex <- path) {
        if (path.indexOf(vertex) != 0){
          totalDistance += dijkstra.getNodesDistance(previousVertex, vertex)
          previousVertex = vertex
        }

        message += vertex.id + " -> "
      }

      message = message take message.length - 4
      message += ": " + totalDistance
    }

    message
  }

  /**
    * Calculate stations nearby for the station read from StdIn
    *
    */
  def calculateStationsNearby(): String ={
    //calculate nearby station within certain range
    val graph = new Graph(nodes, edges)
    val dijkstra = new DijkstraAlgorithm(graph)

    dijkstra.executeAlgorithm(nodes.find(x => x.id == nearbyStation).get)

    //build message for printing
    var message = ""
    for (vertex <- dijkstra.distances.toSeq.sortBy(v => v._2)){
      if (vertex._2 > 0 && vertex._2 <= nearbyDistance){
        message += vertex._1.id + ": " + vertex._2 + ", "
      }
    }

    if (message == ""){
      message = "Error: No nearby stations found in the range " + nearbyDistance + " from " + nearbyStation
    } else {
      message = message take message.length - 2
    }

    message
  }
}
