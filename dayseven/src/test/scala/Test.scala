package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestCrabAlignment extends AnyFlatSpec with Matchers {
  val testinput: List[Int] = List(16,1,2,0,4,2,7,1,2,14)
  getFuelForPosition(testinput, 1) shouldEqual 41
  getFuelForPosition(testinput, 3) shouldEqual 39
  getFuelForPosition(testinput, 10) shouldEqual 71

  val answer = findTheFuelForOptimalPosition(testinput)
  answer shouldEqual 37

  def findTheFuelForOptimalPosition(positions: List[Int]): Int = {
    val fuelcostByPosition = getFuelConsumptionList(testinput)
    fuelcostByPosition.min
  }


  def getFuelConsumptionList(positions: List[Int]): List[Int] = {
    var fuelcostByPosition: List[Int] = List()
    for(position <- 0 to testinput.max) {
      val fuelCost = getFuelForPosition(testinput, position)
      fuelcostByPosition = fuelcostByPosition ::: List(fuelCost)
    }
    fuelcostByPosition
  }

  def getFuelForPosition(positions: List[Int], comparePosition: Int): Int = {
    positions.map(x => Math.abs(x-comparePosition)).sum
  }

}
