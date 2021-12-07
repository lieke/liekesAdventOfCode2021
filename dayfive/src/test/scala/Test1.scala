package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CoordinatesTest extends AnyFlatSpec with Matchers {
  //// a single coordinate
  val coordinateTupleVerticalLine: Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]] = ((0,9),(5,9))
  val coordinateTupleHorizontalLine: Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]] = ((0,0),(0,5))
  val diagonalLine: Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]] = ((2,0),(0,2))
  val wrongdiagonalline: Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]] = ((2,0),(3,2))
  val backwardLine: Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]] = ((9,4),(3,4))
  val verticalLineList: List[Tuple2[Int, Int]] = createAllTheLineCoordinates(coordinateTupleVerticalLine)
  "A vertical tuple" should "create a list with all coordinates of that line" in {
    verticalLineList shouldEqual List((0,9), (1,9), (2,9), (3,9), (4,9), (5,9))
  }
  val horizontalLineList: List[Tuple2[Int, Int]] = createAllTheLineCoordinates(coordinateTupleHorizontalLine)
  "A horizontal tuple" should "create a list with all coordinates of that line" in {
    horizontalLineList shouldEqual List((0,0), (0,1), (0,2), (0,3), (0,4), (0,5))
  }
  val diagonalLineList: List[Tuple2[Int, Int]] = createAllTheLineCoordinates(diagonalLine)
  "A diagonal tuple" should "create an empty list" in {
    diagonalLineList shouldEqual List()
  }
  val backwardLineList: List[Tuple2[Int, Int]] = createAllTheLineCoordinates(backwardLine)
  "A backward tuple" should "also create a list" in {
    backwardLineList shouldEqual List((3,4), (4,4), (5,4), (6,4), (7,4), (8,4), (9,4))
  }

  //// start working on a list of coordinates
  val listOfCoordinates: List[Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]]] = List(((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2 ),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2)))
  val allCoordinatesWithOccurences = listOfCoordinates.map(createAllTheLineCoordinates).flatten.groupBy(el => el).map(e => (e._1, e._2.length))
  "The amount of coordinates" should "be correctly calculated" in {
    allCoordinatesWithOccurences.get(0,9).get shouldEqual 2
    allCoordinatesWithOccurences.get(7,0).get shouldEqual 1
  }
  val amountOfOverlappingLines = allCoordinatesWithOccurences.collect{ case (key, value) if (value > 1) => (key, value)}.size
  "The amount of overlapping lines" should "also be correctly calculated" in {
    amountOfOverlappingLines shouldEqual 5
  }

  //// now add diagonal lines
  val isAGoodDiagonalLine: Boolean = checkIfItIsAValidDiagonalLine(diagonalLine)
  val isNotGoodDiagonalLine: Boolean = checkIfItIsAValidDiagonalLine(wrongdiagonalline)
  "A diagonal line" should "be in a 45 degree angle" in {
    isAGoodDiagonalLine shouldEqual true
    isNotGoodDiagonalLine shouldEqual false
  }
  val goodDiagonalList = createDiagonalLine(diagonalLine)
  val wrongDiagonalList = createDiagonalLine(wrongdiagonalline)
  "A diagonal line" should "create the right kind of cover points" in {
    goodDiagonalList shouldEqual List((0,2),(1,1),(2,0))
    wrongDiagonalList shouldEqual List()
  }

  def createDiagonalLine(coordinate:Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]]): List[Tuple2[Int,Int]] = {
    var result: List[Tuple2[Int,Int]] = List()
    if(checkIfItIsAValidDiagonalLine(coordinate)) {
      var x = coordinate._1._1
      var y = coordinate._1._2
      val endX = coordinate._2._1
      val endY = coordinate._2._2
      while (x != endX) {
        result = (x,y) :: result
        if (x < endX) x+=1 else x-=1
        if (y < endY) y+=1 else y-=1
      }
      result = (endX, endY) :: result
    }
    println(result)
    result
  }

  def checkIfItIsAValidDiagonalLine(coordinateTuple:Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]]): Boolean = {
    val differenceX: Int = coordinateTuple._1._1 - coordinateTuple._2._1
    ((coordinateTuple._1._2 + differenceX == coordinateTuple._2._2)||(coordinateTuple._1._2 - differenceX == coordinateTuple._2._2))
  }

  def createTheTheLineCoordinatesWithDiagonals(coordinateTuple:Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]]):  List[Tuple2[Int, Int]] = {
    var coordinatesToWorkWith = coordinateTuple
    if ((coordinateTuple._1._1 > coordinateTuple._2._1) || (coordinateTuple._1._2 > coordinateTuple._2._2)) {
      coordinatesToWorkWith = coordinateTuple.swap
    }
    coordinatesToWorkWith match {
      case ((start,y1),(end,y2)) if (y1 == y2) => createVerticalLine(start, end, y1)
      case ((x1,start),(x2,end)) if (x1 == x2) => createHorizontalLine(start, end, x1)
      case _ => createDiagonalLine(coordinatesToWorkWith)
    }
  }

  def createAllTheLineCoordinates(coordinateTuple:Tuple2[Tuple2[Int, Int], Tuple2[Int, Int]]):  List[Tuple2[Int, Int]] = {
    var coordinatesToWorkWith = coordinateTuple
    if ((coordinateTuple._1._1 > coordinateTuple._2._1) || (coordinateTuple._1._2 > coordinateTuple._2._2)) {
      coordinatesToWorkWith = coordinateTuple.swap
    }
    coordinatesToWorkWith match {
      case ((start,y1),(end,y2)) if (y1 == y2) => createVerticalLine(start, end, y1)
      case ((x1,start),(x2,end)) if (x1 == x2) => createHorizontalLine(start, end, x1)
      case _ => List()
    }
  }

  def createVerticalLine(start:Int, end:Int, yAxis:Int): List[Tuple2[Int,Int]] = {
    var result: List[Tuple2[Int,Int]] = List()
    for(i <- end to start by -1)
      result = Tuple2(i,yAxis) :: result
    result
  }

  def createHorizontalLine(start:Int, end:Int, xAxis:Int): List[Tuple2[Int,Int]] = {
    var result: List[Tuple2[Int,Int]] = List()
    for(i <- end to start by -1)
      result = Tuple2(xAxis, i) :: result
    result
  }
}
