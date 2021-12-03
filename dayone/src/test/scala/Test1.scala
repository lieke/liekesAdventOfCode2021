import org.junit.Test
import org.junit.Assert.*


class FromAList:
  @Test def becomesDeeper(): Unit = 
    val depthList: List[Int] = List (100, 110, 120, 130, 150)
    assertEquals(4, findAllTheDepths(depthList))

  @Test def becomesLessDeep(): Unit = 
    val depthList: List[Int] = List (100, 90, 80, 70, 60)
    assertEquals(0, findAllTheDepths(depthList))

  @Test def switchesItUp(): Unit = 
    val depthList: List[Int] = List (100, 90, 100, 80, 100)
    assertEquals(2, findAllTheDepths(depthList))

  @Test def staysTheSame(): Unit = 
    val depthList: List[Int] = List (100, 100, 100, 100, 100)
    assertEquals(0, findAllTheDepths(depthList))

class FromASlidingWindow:
  @Test def windowGoesDeeper(): Unit = 
    val depthList: List[Int] = List (100, 110, 120, 130, 150)
    assertEquals(2, findAllTheDepths2(depthList))
  
  @Test def fromTheAdventOfCodeExample(): Unit = 
    val depthList: List[Int] = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    assertEquals(5, findAllTheDepths2(depthList))

def findAllTheDepths(depthList: List[Int]): Int = 
  depthList.sliding(2)
    .filter( t => t(0) < t(1) )
    .map( t => t(0) )
    .toList
    .length

def findAllTheDepths2(depthList: List[Int]): Int = 
  val calculatedWindows = depthList.sliding(3)
    .map( t => t(0) + t(1) + t(2))
    .toList
  findAllTheDepths(calculatedWindows)