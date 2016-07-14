import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

/**
  * Created by sorinmanole on 14/07/2016.
  */
class DijkstraTests extends FunSuite {
  var nodes: List[Vertex] = _
  var edges: List[Edge] = _

  val nodeA = new Vertex("A")
  val nodeB = new Vertex("B")
  val nodeC = new Vertex("C")
  val nodeD = new Vertex("D")
  val nodeE = new Vertex("E")
  nodes = List[Vertex](nodeA, nodeB, nodeC, nodeD, nodeE)

  val edge1 = new Edge("Edge_1", nodeA, nodeB, 240)
  val edge2 = new Edge("Edge_2", nodeA, nodeC, 70)
  val edge3 = new Edge("Edge_3", nodeA, nodeD, 120)
  val edge4 = new Edge("Edge_4", nodeC, nodeB, 60)
  val edge5 = new Edge("Edge_5", nodeD, nodeE, 480)
  val edge6 = new Edge("Edge_6", nodeC, nodeE, 240)
  val edge7 = new Edge("Edge_7", nodeB, nodeE, 210)
  val edge8 = new Edge("Edge_8", nodeE, nodeA, 300)
  edges = List[Edge](edge1, edge2, edge3, edge4, edge5, edge6, edge7, edge8)

  DijkstraService.nodes = nodes
  DijkstraService.edges = edges
  DijkstraService.startStation = "A"
  DijkstraService.endStation = "B"
  DijkstraService.nearbyStation = "A"
  DijkstraService.nearbyDistance = 130

  test("test case shortest path A -> B") {
    assert(DijkstraService.calculateShortestPath().equals("A -> C -> B: 130.0"))
  }

  test("test case nearby stations A within 130") {
    assert(DijkstraService.calculateStationsNearby().equals("C: 70.0, D: 120.0, B: 130.0"))
  }

  test("test case nearby stations A within 10") {
    DijkstraService.nearbyDistance = 10
    assert(DijkstraService.calculateStationsNearby().equals("Error: No nearby stations found in the range 10.0 from A"))
  }

  test("test case shortest path A -> F") {
    val nodeF = new Vertex("F")
    nodes = List[Vertex](nodeA, nodeB, nodeC, nodeD, nodeE, nodeF)
    DijkstraService.nodes = nodes
    DijkstraService.startStation = "A"
    DijkstraService.endStation = "F"

    assert(DijkstraService.calculateShortestPath().equals("Error: No route from A to F"))
  }
}
