import org.junit.Test
import org.junit.Assert.*

class Test1:
  val firstNumber: String = "00100"
  val secondNumber: String = "11110"
  val numberList: List[String] = List(firstNumber, secondNumber, "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
  @Test def parseOneElement(): Unit = 
    val parsedBinaryNumber: List[Int] = createAParsedBinaryNumberFromString(firstNumber)
    assertEquals(0, parsedBinaryNumber(1))
    assertEquals(1, parsedBinaryNumber(2))

  @Test def parseSecondElement(): Unit = 
    val parsedBinaryNumber: List[Int] = createAParsedBinaryNumberFromString(secondNumber)
    assertEquals(1, parsedBinaryNumber(1))
    assertEquals(0, parsedBinaryNumber(4))

  @Test def addTwoElementsTogether(): Unit = 
    val valuesAllAddedUp = createAListWithAllTheValuesAddedUp(numberList)
    assertEquals(7, valuesAllAddedUp(0))
    assertEquals(5, valuesAllAddedUp(1))

  @Test def findMostCommonFirstNumber(): Unit = 
    val totalAmountOfBinaryNumbers = numberList.length
    val valuesAllAddedUp = createAListWithAllTheValuesAddedUp(numberList)
    val amountOf1s = valuesAllAddedUp(0)
    val amountOf0s = totalAmountOfBinaryNumbers - amountOf1s
    assertEquals(7, amountOf1s)
    assertEquals(5, amountOf0s)

  @Test def findBinaryEpsilon(): Unit = 
    val totalAmountOfBinaryNumbers = numberList.length
    val gammaString: String = createAListWithAllTheValuesAddedUp(numberList)
      .map(amountOf1s => if(amountOf1s > totalAmountOfBinaryNumbers - amountOf1s) "1" else "0")
      .mkString("")
    assertEquals("10110", gammaString)
  


def createAParsedBinaryNumberFromString(numberString: String): List[Int] = 
  numberString.map(char => if(char == '0') 0 else 1).toList

def createAListWithAllTheValuesAddedUp(binaryNumberList: List[String]): List[Int] = 
  binaryNumberList
    .map(createAParsedBinaryNumberFromString(_))
    .reduce((numberList1:List[Int], numberList2:List[Int]) =>
      (numberList1, numberList2).zipped.map(_ + _))