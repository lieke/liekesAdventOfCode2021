import org.junit.Test
import org.junit.Assert.*
import scala.util.matching.Regex

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

  @Test def findEpsilonAndGamma(): Unit = 
    val totalAmountOfBinaryNumbers = numberList.length
    val gammaList: List[Int] = createAListWithAllTheValuesAddedUp(numberList)
      .map(amountOf1s => if(amountOf1s > totalAmountOfBinaryNumbers - amountOf1s) 1 else 0)
    val epsilonList: List[Int] = gammaList
      .map(element => if(element == 0) 1 else 0)
    val gamma:Int = Integer.parseInt(gammaList.mkString(""), 2)
    val epsilon:Int = Integer.parseInt(epsilonList.mkString(""), 2)
    assertEquals("10110", gammaList.mkString(""))
    assertEquals("01001", epsilonList.mkString(""))
    assertEquals(22, gamma)
    assertEquals(9, epsilon)
  
  @Test def findThePowerConsumption(): Unit = 
    val powerConsumption:Int = findPowerConsumption(numberList)
    assertEquals(198, powerConsumption)

class Test2:
  val numberList: List[String] = List("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
  
  @Test def allTheSweepTests(): Unit = 
    var oxygenGeneratorMatchList: List[String] = getOxygenGeneratorList(numberList)
    val firstMatchPattern:String = oxygenGeneratorMatchList(0)+"\\d+"
    println(firstMatchPattern)
    val firstSweepList = numberList.filter(x => x.matches(firstMatchPattern))
    assertEquals(firstSweepList.length, 7)

    oxygenGeneratorMatchList = getOxygenGeneratorList(firstSweepList)
    val secondMatchPattern: String = "\\d"+oxygenGeneratorMatchList(1)+"\\d+"
    val secondSweepList = firstSweepList.filter(x => x.matches(secondMatchPattern))
    assertEquals(secondSweepList.length, 4)

    oxygenGeneratorMatchList = getOxygenGeneratorList(secondSweepList)
    val thirdMatchPattern: String = "\\d\\d"+oxygenGeneratorMatchList(2)+"\\d+"
    val thirdSweepList = secondSweepList.filter(x => x.matches(thirdMatchPattern))
    assertEquals(thirdSweepList.length, 3)
  
  @Test def oxygenWithAWhileLoop(): Unit = 
    var resultList: List[String] = numberList
    var oxygenGeneratorMatchList: List[String] = getOxygenGeneratorList(resultList)
    var startOfPattern:String = ""
    var index = 0
    while (resultList.length > 1)
      val patternToMatch:String = startOfPattern + oxygenGeneratorMatchList(index)+ "\\d*"
      println("Sweep numer " + index + " - pattern " + patternToMatch)
      resultList = resultList.filter(x => x.matches(patternToMatch))
      println("New result " + resultList)
      oxygenGeneratorMatchList = getOxygenGeneratorList(resultList)
      startOfPattern = startOfPattern + "\\d"
      index = index + 1
    val result:Int = Integer.parseInt(resultList(0),2)
    assertEquals(index, 5)
    assertEquals(resultList(0), "10111")
    assertEquals(result, 23)

  @Test def scrubberWithAWhileLoop(): Unit = 
    var resultList: List[String] = numberList
    var scrubberMatchList: List[String] = getCO2ScrubberList(resultList)
    var startOfPattern:String = ""
    var index = 0
    while (resultList.length > 1)
      val patternToMatch:String = startOfPattern + scrubberMatchList(index)+ "\\d*"
      println("Sweep numer " + index + " - pattern " + patternToMatch)
      resultList = resultList.filter(x => x.matches(patternToMatch))
      println("New result " + resultList)
      scrubberMatchList = getCO2ScrubberList(resultList)
      startOfPattern = startOfPattern + "\\d"
      index = index + 1
    val result:Int = Integer.parseInt(resultList(0),2)
    assertEquals(index, 3)
    assertEquals(resultList(0), "01010")
    assertEquals(result, 10)


def findPowerConsumption(binaryNumbersList: List[String]): Int = 
  val gammaList: List[Int] = getGammaList(binaryNumbersList)
  val epsilonList: List[Int] = getEpsilonList(gammaList)
  createDecimalNumberFromList(gammaList) * createDecimalNumberFromList(epsilonList)

def getGammaList(binaryNumberList: List[String]): List[Int] = 
  val totalAmountOfBinaryNumbers = binaryNumberList.length
  createAListWithAllTheValuesAddedUp(binaryNumberList)
    .map(amountOf1s => if(amountOf1s > (totalAmountOfBinaryNumbers - amountOf1s)) 1 else 0)

def getOxygenGeneratorList(binaryNumberList: List[String]): List[String] = 
  val totalAmountOfBinaryNumbers = binaryNumberList.length
  createAListWithAllTheValuesAddedUp(binaryNumberList)
    .map(amountOf1s => if(amountOf1s >= (totalAmountOfBinaryNumbers - amountOf1s)) "1" else "0")

def getCO2ScrubberList(binaryNumberList: List[String]): List[String] = 
  val totalAmountOfBinaryNumbers = binaryNumberList.length
  createAListWithAllTheValuesAddedUp(binaryNumberList)
    .map(amountOf1s => if(amountOf1s >= (totalAmountOfBinaryNumbers - amountOf1s)) "0" else "1")

def getEpsilonList(gammaList: List[Int]): List[Int] = 
  gammaList.map(element => if(element == 0) 1 else 0)

def createDecimalNumberFromList(binaryAsList: List[Int]): Int = 
  Integer.parseInt(binaryAsList.mkString(""), 2)

def createAParsedBinaryNumberFromString(numberString: String): List[Int] = 
  numberString.map(char => if(char == '0') 0 else 1).toList

def createAListWithAllTheValuesAddedUp(binaryNumberList: List[String]): List[Int] = 
  binaryNumberList
    .map(createAParsedBinaryNumberFromString(_))
    .reduce((numberList1:List[Int], numberList2:List[Int]) =>
      (numberList1, numberList2).zipped.map(_ + _))