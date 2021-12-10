package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Test1 extends AnyFlatSpec with Matchers {
  val lanternList: List[Int] = List(3,4,3,1,2)
  val lanternList2: List[Int] = List(6,0,6,4,5,6,7,8,8)
  val amountOfFish = findTheAmountOfFish(lanternList, 80)
  amountOfFish shouldEqual 5934

  def findTheAmountOfFish(lanternList: List[Int], amountOfDays: Int): Int = {
    var nextDayList = lanternList
    for(i <- 1 to amountOfDays) {
      nextDayList = nextDay(nextDayList)
    }
    nextDayList.length
  }

  def nextDay(oldLanternList: List[Int]): List[Int] = {
    val sortedList = oldLanternList.sorted
    val indexOf0 = sortedList.indexOf(0)
    val difference = sortedList.lastIndexOf(0) - indexOf0
    val amountOfNewMembers = if(indexOf0 > -1) difference+1 else 0
    val newMembers = List.fill(if(amountOfNewMembers>0)amountOfNewMembers else 0)(8)
    val nextDayList = oldLanternList.map {case 0 => 6; case x => x-1 }
    nextDayList ::: newMembers
  }
}

