import org.junit.Test
import org.junit.Assert.*


class ParseInput:
  val input = List(("forward", 5), ("down" , 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2))
  @Test def parseForward(): Unit = 
    val xAxisValue = getDirectionValue(input, "forward")
    assertEquals(15, xAxisValue)
  @Test def parseDown(): Unit = 
    val zAxisPositiveValue = getDirectionValue(input, "down")
    assertEquals(13, zAxisPositiveValue)
  @Test def parseUp(): Unit = 
    val zAxisNegativeValue = getDirectionValue(input, "up")
    assertEquals(3, zAxisNegativeValue)
  @Test def parseAll(): Unit =
    val result = getTheResult(input)
    assertEquals(150, result)
    
def getDirectionValue(input: List[Tuple2[String, Int]], directionName: String): Int = input
      .filter(_._1 == directionName)
      .map(_._2)
      .foldLeft(0)(_+_)

def getTheResult(input: List[Tuple2[String, Int]]) :Int = 
  val xAxisValue = getDirectionValue(input, "forward")
  val zAxisPositiveValue = getDirectionValue(input, "down")
  val zAxisNegativeValue = getDirectionValue(input, "up")
  xAxisValue * (zAxisPositiveValue - zAxisNegativeValue)
    