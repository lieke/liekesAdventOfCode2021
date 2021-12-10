package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestLavaLevels extends AnyFlatSpec with Matchers {
  val testinput: List[String] = List("2199943210", "3987894921", "9856789892", "8767896789", "9899965678")
  val riskMap = createRiskmap(testinput)
  "The riskmap" should "be correctly corresponding to the risk levels at each point" in {
    riskMap((0, 0)) shouldEqual 3
    riskMap((9, 0)) shouldEqual 1
    riskMap((0, 4)) shouldEqual 10
    riskMap((9, 4)) shouldEqual 9
  }

  findRiskLevelOfALowPoint((1, 0), riskMap) shouldEqual 2
  findRiskLevelOfALowPoint((0, 0), riskMap) shouldEqual 0
  findRiskLevelOfALowPoint((9, 0), riskMap) shouldEqual 1
  findRiskLevelOfALowPoint((6, 4), riskMap) shouldEqual 6

  val sumOfScoresOfLowPoints = riskMap.keys.toList.map(coordinate => findRiskLevelOfALowPoint(coordinate, riskMap)).sum
  sumOfScoresOfLowPoints shouldEqual 15

  def findRiskLevelOfALowPoint(coordinate: Tuple2[Int, Int], riskMap: Map[Tuple2[Int, Int], Int]): Int = {
    val xCoordinate = coordinate._1
    val yCoordinate = coordinate._2
    val coordinateRiskLevel = riskMap(coordinate)
    var surroundingRiskLevels: List[Int] = List(riskMap((xCoordinate, yCoordinate - 1))) //above
    surroundingRiskLevels = riskMap((xCoordinate, yCoordinate + 1)) :: surroundingRiskLevels //under
    surroundingRiskLevels = riskMap((xCoordinate - 1, yCoordinate)) :: surroundingRiskLevels //left
    surroundingRiskLevels = riskMap((xCoordinate + 1, yCoordinate)) :: surroundingRiskLevels //right
    surroundingRiskLevels = surroundingRiskLevels.filter(_ != 9999).sorted
    if (coordinateRiskLevel < surroundingRiskLevels(0)) coordinateRiskLevel else 0
  }

  def createRiskmap(input: List[String]): Map[Tuple2[Int, Int], Int] = {
    var riskMap: Map[Tuple2[Int, Int], Int] = Map().withDefaultValue(9999)
    for (yAxis <- 0 to input.length - 1) {
      val row: String = input(yAxis)
      for (xAxis <- 0 to row.length - 1) {
        val riskLevel: Int = row.charAt(xAxis).asDigit + 1
        riskMap += ((xAxis, yAxis) -> riskLevel)
      }
    }
    riskMap
  }
}
