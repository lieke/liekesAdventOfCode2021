package example

object Main extends Greeting with App {
  val input = List(1,4,1,1,1,1,1,1,1,4,3,1,1,3,5,1,5,3,2,1,1,2,3,1,1,5,3,1,5,1,1,2,1,2,1,1,3,1,5,1,1,1,3,1,1,1,1,1,1,4,5,3,1,1,1,1,1,1,2,1,1,1,1,4,4,4,1,1,1,1,5,1,2,4,1,1,4,1,2,1,1,1,2,1,5,1,1,1,3,4,1,1,1,3,2,1,1,1,4,1,1,1,5,1,1,4,1,1,2,1,4,1,1,1,3,1,1,1,1,1,3,1,3,1,1,2,1,4,1,1,1,1,3,1,1,1,1,1,1,2,1,3,1,1,1,1,4,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,5,1,1,1,2,2,1,1,3,5,1,1,1,1,3,1,3,3,1,1,1,1,3,5,2,1,1,1,1,5,1,1,1,1,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,1,1,1,1,5,1,4,3,3,1,3,4,1,1,1,1,1,1,1,1,1,1,4,3,5,1,1,1,1,1,1,1,1,1,1,1,1,1,5,2,1,4,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,1,1,1,1,1,1,1,2,1,4,4,1,1,1,1,1,1,1,5,1,1,2,5,1,1,4,1,3,1,1)

  println(greeting)
  println("The amount of fish after 80 days is: " + findTheAmountOfFish(input, 80))
  println("The amount of fish after 256 days is: " + findTheAmountOfFish(input, 256))

  def findTheAmountOfFish(lanternList: List[Int], amountOfDays: Int): Int = {
    var nextDayList = lanternList
    for(i <- 1 to amountOfDays) {
      if (i == 75) println("75 days")
      if (i == 100) println("100 days")
      if (i == 125) println("125 days")
      if (i == 150) println("150 days")
      if (i == 175) println("175 days")
      if (i == 200) println("200 days")
      if (i == 225) println("225 days")
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

trait Greeting {
  lazy val greeting: String = "Welcome to day six of the Advent of Code 2021"
}
