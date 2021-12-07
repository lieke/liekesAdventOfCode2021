import org.junit.Test
import org.junit.Assert.*
import scala.collection.mutable.HashMap

class TheStart:
  val allCards = List(List(List(22,13,17,11,0), List(8,2,23,4,24), List(21,9,14,16,7), List(6,10,3,18,5), List(1,12,20,15,19)),List(List(3,15,0,2,22), List(9,18,13,17,5), List(19,8,7,25,23), List(20,11,10,24,4), List(14,21,16,12,6)),List(List(14,21,17,24,4), List(10,16,15,9,19), List(18,8,23,26,20), List(22,11,13,6,5), List(2,0,12,3,7)))
  @Test def testBingoRow(): Unit = 
    val sampleRowBingo:Map[Int, Boolean] = Map(0 -> true, 13 -> true, 17 -> true, 22 -> true, 11 -> true)
    val sampleRowNotBingo:Map[Int, Boolean] = Map(0 -> false, 13 -> true, 17 -> true, 22 -> true, 11 -> true)
    assertTrue(isRowBingo(sampleRowBingo))
    assertFalse(isRowBingo(sampleRowNotBingo))

  @Test def testIsBingoCard(): Unit = 
    val sampleCardBingo:List[Map[Int, Boolean]] = List(Map(0 -> true, 13 -> true, 17 -> true, 22 -> true, 11 -> true), Map(24 -> false, 2 -> false, 23 -> false, 8 -> false, 4 -> false), Map(14 -> true, 21 -> false, 9 -> false, 7 -> false, 16 -> false), Map(5 -> false, 10 -> false, 6 -> false, 3 -> false, 18 -> false), Map(20 -> false, 1 -> false, 12 -> false, 19 -> false, 15 -> false))
    val sampleCardNotBingo:List[Map[Int, Boolean]] = List(Map(0 -> false, 13 -> true, 17 -> true, 22 -> true, 11 -> true), Map(24 -> false, 2 -> false, 23 -> false, 8 -> false, 4 -> false), Map(14 -> false, 21 -> false, 9 -> false, 7 -> false, 16 -> false), Map(5 -> false, 10 -> false, 6 -> false, 3 -> false, 18 -> false), Map(20 -> false, 1 -> false, 12 -> false, 19 -> false, 15 -> false))
    assertTrue(isCardBingo(sampleCardBingo))
    assertFalse(isCardBingo(sampleCardNotBingo))

  @Test def theBingoTestRow(): Unit = 
    val theDraws = List(7,4,9,5,11,17,23,2,0,14,21)
    var allCardsWithMarkings: List[List[Map[Int, Boolean]]] = createSetupForMarkings(allCards)
    for (draw <- theDraws)
      allCardsWithMarkings = markADraw(allCardsWithMarkings, draw)
    var bingoCard = findBingo(allCardsWithMarkings)
    assertEquals(-1, bingoCard)
    allCardsWithMarkings = markADraw(allCardsWithMarkings, 24)
    bingoCard = findBingo(allCardsWithMarkings)
    assertEquals(2, bingoCard)

  @Test def theBingoTestColumn(): Unit = 
    val theDraws = List(3,9,19,20)
    var allCardsWithMarkings: List[List[Map[Int, Boolean]]] = createSetupForMarkings(allCards)
    for (draw <- theDraws)
      allCardsWithMarkings = markADraw(allCardsWithMarkings, draw)
    var bingoCard = findBingo(allCardsWithMarkings)
    assertEquals(-1, bingoCard)
    allCardsWithMarkings = markADraw(allCardsWithMarkings, 14)
    bingoCard = findBingo(allCardsWithMarkings)
    assertEquals(1, bingoCard)

  @Test def theFinalTest(): Unit = 
    val theDraws = List(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
    val theWinningCard = findTheWinningBingoCard(allCards, theDraws)
    assertEquals(2, theWinningCard)
    val theScore = findTheScore(allCards, theDraws)
    assertEquals(4512, theScore)

  @Test def findTheLooserTest(): Unit = {
    val theDraws = List(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
    var theMutatingDraws = theDraws
    var allCardsWithMarkings: List[List[Map[Int, Boolean]]] = createSetupForMarkings(allCards)
    var theLoosingCard = -1
    while (theLoosingCard < 0) {
      theLoosingCard = findTheLoosingBingoCard(allCardsWithMarkings, theMutatingDraws)
      theMutatingDraws = theMutatingDraws.slice(0, theMutatingDraws.length-1)
    }
    val theFinalMarkingToDetermineLooser = theDraws(theMutatingDraws.length+1)
    val markedLoosingCard = markAllDrawsOnACard(allCardsWithMarkings(theLoosingCard).slice(0,5), theDraws.slice(0, theMutatingDraws.length+2))
    val onlyTheValuesOfLoosingCard = markedLoosingCard.map(row => row.collect{
      case (key, false) => (key, false)})
    val sum = onlyTheValuesOfLoosingCard.foldLeft(Set[Int]())(_ ++ _.keySet).foldLeft(0)(_ + _)
    val theScore = sum * theFinalMarkingToDetermineLooser
    assertEquals(1, theLoosingCard)
    assertEquals(13, theFinalMarkingToDetermineLooser)
    assertEquals(148, sum)
    assertEquals(1924, theScore)
  }

  def findTheLoosingBingoCard(allCards: List[List[Map[Int, Boolean]]], theDraws:List[Int]): Int =
    var allCardsWithMarkings = allCards
    var whereIsNoBingo: Int = -1
    for (draw <- theDraws)
      allCardsWithMarkings = markADraw(allCardsWithMarkings, draw)
    findNotBingo(allCardsWithMarkings)

  def findTheScore(allCards: List[List[List[Int]]], allDraws: List[Int]): Int =
    var allCardsWithMarkings: List[List[Map[Int, Boolean]]] = createSetupForMarkings(allCards)
    var thereIsBingo: Int = -1
    var nextDraw = -1
    while(thereIsBingo < 0)
      nextDraw += 1
      allCardsWithMarkings = markADraw(allCardsWithMarkings, allDraws(nextDraw))
      thereIsBingo = findBingo(allCardsWithMarkings)
    val theWinningCard = allCardsWithMarkings(thereIsBingo)
    val onlyTheValuesOfWinningCard = theWinningCard.slice(0,5).map(row => row.collect{
        case (key, false) => (key, false)})
    val sum = onlyTheValuesOfWinningCard.foldLeft(Set[Int]())(_ ++ _.keySet).foldLeft(0)(_ + _)
    sum * allDraws(nextDraw)

  def markAllDrawsOnACard(card:List[Map[Int, Boolean]],draws:List[Int]): List[Map[Int, Boolean]] =
    var result = card
    for (draw <- draws) {
      result = result.map(row => row.map((value, marking) => if (value == draw) (value, true) else (value, marking)))
    }
    result

  def findTheWinningBingoCard(allCards: List[List[List[Int]]], allDraws: List[Int]): Int =
    var allCardsWithMarkings: List[List[Map[Int, Boolean]]] = createSetupForMarkings(allCards)
    var thereIsBingo: Int = -1
    var nextDraw = 0
    while(thereIsBingo < 0)
      allCardsWithMarkings = markADraw(allCardsWithMarkings, allDraws(nextDraw))
      thereIsBingo = findBingo(allCardsWithMarkings)
      nextDraw += 1
    thereIsBingo

  def findBingo(allCardsWithMarkings: List[List[Map[Int, Boolean]]]): Int =
    var result = -1
    for(x <- 0 to allCardsWithMarkings.length-1)
      if (isCardBingo(allCardsWithMarkings(x))) result = x
    result

  def findNotBingo(allCardsWithMarkings: List[List[Map[Int, Boolean]]]): Int =
    var result = -1
    for(x <- 0 to allCardsWithMarkings.length-1)
      if (!isCardBingo(allCardsWithMarkings(x))) result = x
    result

  def markADraw(allCardsWithMarkings: List[List[Map[Int, Boolean]]], theDraw:Int): List[List[Map[Int, Boolean]]] = 
    allCardsWithMarkings
      .map(card => card
        .map(row => row
          .map((value, marking) => if (value == theDraw) (value, true) else (value, marking))))

  def createSetupForMarkings(allCards: List[List[List[Int]]]): List[List[Map[Int, Boolean]]] = 
    allCards
      .map(card => createSetupForSingleCard(card))
        
  def createSetupForSingleCard(cardWithoutMarkings: List[List[Int]]): List[Map[Int, Boolean]] =
    var allColumns: List[Map[Int, Boolean]] = List()
    for (yAxis <- 0 to cardWithoutMarkings.length-1)
      var columnMap: Map[Int, Boolean] = Map()
      for (xAxis <- 0 to cardWithoutMarkings.length-1)
        columnMap = columnMap + (cardWithoutMarkings(xAxis)(yAxis) -> false)
      allColumns = columnMap :: allColumns
    var allRows: List[Map[Int, Boolean]] = cardWithoutMarkings.map(row => row.map(element => element -> false).toMap)
    allRows ::: allColumns

  def isCardBingo(card:List[Map[Int,Boolean]]): Boolean =
    card.map(isRowBingo(_)).contains(true)
  
  def isRowBingo(row: Map[Int, Boolean]): Boolean =
    !row.values.exists(_ == false)

  
