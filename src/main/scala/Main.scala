/**
  * Created by sorinmanole on 14/07/2016.
  */

object Main extends App{
  def execute(): Unit ={
    println("***INPUT***")
    DijkstraService.readGraph()
    DijkstraService.readRouteToCalculatePath()
    DijkstraService.readStationToCalculateNearby()
    println()

    println("***OUTPUT***")
    println(DijkstraService.calculateShortestPath())
    println(DijkstraService.calculateStationsNearby())
  }

  execute()
}
