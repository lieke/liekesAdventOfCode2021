import org.junit.Test
import org.junit.Assert.*


class ParseInputAgain:
  val input = List(("forward", 5), ("down" , 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2))
  @Test def parseFirstElement(): Unit = 
    val firstElement = input(0)
    var position =  (0, 0, 0)
    val newPosition = calculateNewPosition(position, firstElement)
    assertEquals(5, newPosition._1)
    assertEquals(0, newPosition._2)
    assertEquals(0, newPosition._3)
  @Test def parseSecondElement(): Unit = 
    val secondElement = input(1)
    var position =  (5, 0, 0)
    val newPosition = calculateNewPosition(position, secondElement)
    assertEquals(5, newPosition._1)
    assertEquals(5, newPosition._2)
    assertEquals(0, newPosition._3)
  @Test def parseThirdElement(): Unit = 
    val thirdElement = input(2)
    var position =  (5, 5, 0)
    val newPosition = calculateNewPosition(position, thirdElement)
    assertEquals(13, newPosition._1)
    assertEquals(5, newPosition._2)
    assertEquals(40, newPosition._3)
  @Test def parseFourthElement(): Unit = 
    val fourthElement = input(3)
    var position =  (13, 5, 40)
    val newPosition = calculateNewPosition(position, fourthElement)
    assertEquals(13, newPosition._1)
    assertEquals(2, newPosition._2)
    assertEquals(40, newPosition._3)
  @Test def parseAllInputs(): Unit = 
    var position = (0, 0, 0)
    for (newDirection <- input) {
      position = calculateNewPosition(position, newDirection)
    }
    assertEquals(15, position._1)
    assertEquals(10, position._2)
    assertEquals(60, position._3)
  @Test def doTheEntireThing(): Unit = 
    val result: Int = getResult2(input)
    assertEquals(900, result)


def getResult2(input: List[Tuple2[String, Int]]): Int = {
  var position = (0, 0, 0)
  for (newDirection <- input) {
    position = calculateNewPosition(position, newDirection)
  }
  position._1 * position._3
} 

//position(horizontalPosition, aim, depth)
def calculateNewPosition(position:Tuple3[Int, Int, Int], elementToBeParsed:Tuple2[String, Int]): Tuple3[Int, Int, Int] =
  var horizontalPosition = position._1
  var aim = position._2
  var depth = position._3
  elementToBeParsed match {
    case ("forward", value:Int) => {
        horizontalPosition += value
        depth += aim * value }
    case ("up", value:Int) => aim -= value
    case ("down", value:Int) => aim += value
    case _ =>
  }
  return (horizontalPosition, aim, depth)
