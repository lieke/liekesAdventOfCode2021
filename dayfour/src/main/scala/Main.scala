
@main def hello: Unit = 
  val theWinningCard = findTheWinningBingoCard(allCards, theDraws)
  val theScore = findTheScore(allCards, theDraws)
  println("The score of the winning card is: " + theScore)
  val theLoosingScore = findTheLoosingScore(allCards, theDraws)
  println("The score of the loosing card is: " + theLoosingScore)

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

def findTheLoosingScore(allCards: List[List[List[Int]]], allDraws: List[Int]): Int =
  var theMutatingDraws = allDraws
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
  sum * theFinalMarkingToDetermineLooser

def markAllDrawsOnACard(card:List[Map[Int, Boolean]],draws:List[Int]): List[Map[Int, Boolean]] =
  var result = card
  for (draw <- draws) {
    result = result.map(row => row.map((value, marking) => if (value == draw) (value, true) else (value, marking)))
  }
  result

def findTheLoosingBingoCard(allCards: List[List[Map[Int, Boolean]]], theDraws:List[Int]): Int =
  var allCardsWithMarkings = allCards
  var whereIsNoBingo: Int = -1
  for (draw <- theDraws)
    allCardsWithMarkings = markADraw(allCardsWithMarkings, draw)
  findNotBingo(allCardsWithMarkings)


def findNotBingo(allCardsWithMarkings: List[List[Map[Int, Boolean]]]): Int =
  var result = -1
  for(x <- 0 to allCardsWithMarkings.length-1)
    if (!isCardBingo(allCardsWithMarkings(x))) result = x
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

val allCards = List(List(List(61,96,92,39,0),List(35,25,50,22,60),List(3,88,69,48,62),List(75,24,97,51,67),List(87,74,94,77,83)),List(List(1,70,59,40,55),List(42,88,10,17,80),List(27,24,82,45,23),List(5,19,48,51,11),List(75,72,97,74,7)),List(List(58,40,78,83,74),List(4,94,17,63,62),List(55,61,5,27,69),List(99,84,89,81,59),List(64,28,91,49,97)),List(List(92,88,51,12,22),List(0,5,65,32,77),List(80,40,3,10,90),List(91,47,58,57,14),List(86,71,94,36,75)),List(List(71,24,16,66,29),List(8,47,93,68,36),List(42,67,69,55,15),List(75,6,34,60,70),List(95,92,14,0,81)),List(List(52,49,37,41,67),List(9,8,2,13,17),List(92,89,38,16,53),List(63,46,60,4,87),List(57,96,77,85,39)),List(List(84,98,52,95,89),List(81,67,99,85,50),List(88,11,76,49,8),List(4,30,51,78,20),List(70,64,74,40,79)),List(List(45,65,87,79,14),List(11,26,98,70,28),List(46,85,54,55,48),List(97,59,62,57,16),List(30,40,95,7,18)),List(List(97,25,38,1,26),List(20,86,7,68,39),List(2,55,29,33,65),List(46,14,72,47,18),List(60,48,41,9,50)),List(List(71,81,15,49,50),List(72,28,51,11,35),List(20,7,36,84,65),List(93,33,14,47,45),List(89,0,75,60,16)),List(List(98,90,47,94,55),List(69,41,81,1,43),List(73,95,65,15,80),List(85,99,60,92,0),List(13,33,82,51,22)),List(List(47,58,82,67,30),List(88,23,64,4,39),List(94,52,61,1,75),List(3,8,34,87,49),List(13,38,60,54,35)),List(List(91,62,88,29,33),List(84,27,6,18,11),List(47,87,58,42,34),List(69,46,75,40,43),List(63,97,53,49,66)),List(List(80,57,73,65,44),List(95,55,27,46,10),List(82,24,90,97,75),List(33,41,31,84,9),List(5,48,18,49,12)),List(List(92,63,91,14,13),List(32,12,66,87,79),List(44,60,7,96,84),List(58,41,42,3,27),List(16,59,43,77,11)),List(List(80,36,53,56,62),List(26,8,4,79,51),List(22,91,69,78,2),List(59,13,23,81,93),List(30,16,49,33,65)),List(List(52,88,12,67,85),List(74,78,75,72,79),List(81,26,82,5,0),List(23,56,41,3,32),List(31,69,15,66,87)),List(List(22,71,80,0,63),List(94,31,13,60,42),List(41,77,90,92,91),List(64,95,5,23,73),List(85,15,3,88,10)),List(List(72,75,88,52,38),List(17,86,54,79,87),List(66,61,51,3,26),List(68,47,89,11,41),List(50,33,92,7,81)),List(List(82,80,9,65,34),List(3,49,42,36,76),List(95,94,61,32,43),List(72,67,56,45,54),List(77,48,14,6,25)),List(List(44,75,99,62,11),List(43,73,2,87,83),List(96,63,85,14,30),List(32,70,18,29,55),List(1,88,15,27,24)),List(List(2,38,46,61,7),List(45,19,97,31,54),List(88,40,14,81,87),List(69,39,32,16,21),List(22,5,0,29,92)),List(List(78,57,85,4,70),List(82,43,12,69,79),List(60,34,15,63,45),List(90,77,93,31,47),List(27,49,25,71,19)),List(List(49,10,40,51,45),List(9,44,86,26,27),List(93,98,22,63,95),List(88,66,33,74,57),List(81,24,28,91,72)),List(List(14,83,60,54,57),List(18,15,41,4,47),List(39,98,62,33,5),List(30,70,6,91,90),List(86,21,28,84,81)),List(List(91,46,49,9,32),List(85,33,87,83,76),List(17,14,37,94,6),List(31,13,92,89,78),List(15,66,47,74,63)),List(List(55,6,83,19,96),List(71,22,88,99,50),List(89,84,26,45,38),List(57,77,87,93,25),List(44,49,16,64,34)),List(List(79,76,46,19,51),List(85,90,58,29,3),List(34,2,81,62,99),List(84,60,78,91,96),List(4,27,43,47,98)),List(List(66,2,38,39,37),List(35,25,51,10,82),List(91,62,1,12,93),List(83,29,47,32,56),List(74,19,50,95,49)),List(List(59,57,35,50,51),List(27,38,62,76,3),List(52,49,83,75,4),List(64,16,93,7,91),List(40,17,65,41,97)),List(List(18,37,45,44,4),List(72,7,28,0,75),List(9,2,95,90,38),List(24,79,93,22,88),List(94,70,57,6,20)),List(List(11,61,65,50,23),List(74,51,80,91,22),List(5,32,27,57,14),List(59,86,70,17,10),List(21,62,20,18,67)),List(List(98,9,88,79,78),List(99,56,91,41,67),List(17,39,65,16,38),List(75,84,11,21,61),List(22,81,52,55,87)),List(List(45,36,74,47,19),List(15,22,88,85,32),List(38,63,54,16,13),List(29,7,48,90,43),List(68,3,24,17,30)),List(List(72,77,68,75,57),List(43,74,32,61,34),List(37,2,47,25,85),List(56,12,95,98,0),List(80,36,39,22,11)),List(List(77,58,24,57,99),List(70,16,33,41,94),List(54,61,20,90,30),List(29,17,55,0,83),List(13,37,42,49,38)),List(List(86,58,13,11,6),List(73,26,25,0,67),List(56,44,87,5,49),List(4,91,51,66,22),List(28,8,1,15,57)),List(List(61,24,50,25,66),List(92,42,98,55,96),List(46,79,22,33,91),List(97,0,69,90,54),List(17,38,34,39,52)),List(List(68,28,67,45,87),List(8,80,52,41,54),List(34,47,4,78,59),List(10,29,32,11,26),List(17,33,7,93,35)),List(List(10,15,33,46,14),List(6,56,52,16,92),List(47,36,17,8,69),List(77,45,73,84,9),List(55,60,80,44,64)),List(List(58,18,25,11,83),List(75,7,53,42,68),List(48,52,6,0,43),List(80,97,16,60,1),List(29,67,15,5,17)),List(List(77,55,54,24,66),List(58,2,4,39,12),List(57,86,69,91,8),List(67,84,65,13,20),List(87,59,40,34,27)),List(List(39,7,40,77,91),List(13,76,32,92,56),List(34,17,81,27,66),List(37,80,83,85,15),List(43,36,30,26,63)),List(List(11,50,72,85,34),List(3,92,58,53,7),List(98,10,49,97,12),List(26,42,14,24,56),List(28,20,59,54,4)),List(List(55,56,29,80,96),List(63,68,44,22,12),List(65,4,95,6,26),List(21,35,14,87,8),List(17,92,86,30,53)),List(List(9,42,20,37,19),List(65,46,11,54,92),List(52,4,56,80,99),List(41,55,43,90,17),List(60,87,13,50,3)),List(List(3,29,4,41,95),List(14,9,11,23,10),List(7,63,68,58,66),List(13,46,67,86,51),List(28,36,0,73,84)),List(List(45,1,19,74,36),List(58,64,30,86,83),List(99,42,70,97,54),List(17,75,56,80,81),List(93,41,90,10,88)),List(List(24,25,0,94,22),List(70,1,50,10,14),List(89,77,76,63,46),List(33,72,81,28,60),List(68,40,12,31,20)),List(List(79,33,30,55,71),List(31,91,54,0,82),List(10,78,9,49,14),List(85,72,5,3,24),List(86,38,97,46,61)),List(List(20,84,97,52,79),List(45,73,11,18,58),List(63,86,21,9,87),List(48,90,13,77,49),List(44,85,56,71,55)),List(List(16,1,54,13,83),List(38,32,69,28,43),List(5,50,57,95,47),List(34,76,45,74,89),List(46,91,71,39,17)),List(List(82,45,14,28,57),List(27,21,17,29,51),List(95,32,31,80,91),List(89,74,67,76,79),List(6,0,4,43,94)),List(List(52,66,44,74,95),List(85,51,79,76,54),List(89,34,59,10,27),List(45,6,69,98,48),List(88,19,3,65,94)),List(List(61,9,67,72,71),List(93,48,64,52,11),List(74,85,12,13,23),List(41,4,94,16,57),List(63,88,28,89,40)),List(List(68,23,54,56,44),List(13,77,26,2,46),List(28,81,15,16,62),List(82,51,71,86,72),List(99,0,52,41,32)),List(List(99,38,7,87,9),List(69,96,22,57,24),List(64,81,29,67,14),List(48,52,6,88,92),List(90,44,51,40,8)),List(List(41,1,23,24,73),List(10,4,66,60,22),List(17,9,69,53,63),List(42,34,99,86,56),List(75,82,81,18,79)),List(List(58,64,12,59,30),List(21,94,28,77,53),List(88,90,97,62,83),List(35,70,27,98,26),List(65,34,25,73,75)),List(List(81,7,90,91,74),List(23,34,67,31,50),List(60,87,5,40,77),List(69,93,27,49,53),List(39,62,68,16,89)),List(List(82,13,28,65,35),List(5,42,90,12,51),List(15,85,64,86,25),List(87,22,88,37,98),List(39,10,46,56,49)),List(List(62,25,93,75,34),List(42,89,27,36,18),List(32,54,59,26,6),List(51,19,47,85,95),List(33,39,73,29,79)),List(List(15,27,0,79,69),List(13,73,25,19,43),List(30,8,46,34,58),List(4,86,66,74,18),List(83,33,92,11,47)),List(List(45,25,22,14,4),List(83,3,65,17,85),List(91,26,5,19,87),List(66,89,29,49,64),List(52,20,58,93,53)),List(List(30,64,52,14,34),List(63,16,97,9,15),List(2,72,65,45,17),List(47,98,77,23,0),List(50,20,38,60,26)),List(List(46,67,84,66,55),List(7,32,31,75,19),List(71,85,37,12,52),List(39,27,8,81,44),List(89,47,42,16,58)),List(List(74,99,81,86,89),List(92,20,7,58,30),List(63,96,25,45,2),List(97,50,94,33,87),List(38,6,51,21,62)),List(List(52,27,20,32,19),List(17,80,70,92,96),List(49,44,62,60,94),List(40,28,86,4,7),List(38,91,3,77,29)),List(List(8,28,89,99,6),List(46,54,34,95,3),List(88,60,29,91,10),List(42,13,62,94,76),List(56,52,72,85,59)),List(List(85,50,42,5,91),List(67,7,21,6,56),List(14,8,70,10,78),List(77,80,57,29,96),List(17,23,73,16,38)),List(List(59,61,47,43,13),List(7,93,11,72,83),List(0,96,67,27,2),List(42,5,41,65,94),List(40,34,33,50,3)),List(List(25,79,52,11,94),List(73,14,7,99,19),List(92,40,2,28,45),List(55,34,87,24,96),List(36,16,66,78,35)),List(List(11,27,90,50,55),List(68,84,63,57,89),List(35,14,29,77,24),List(92,81,7,1,85),List(99,64,20,2,49)),List(List(20,66,85,88,57),List(49,17,78,1,80),List(18,24,11,31,65),List(30,34,45,99,19),List(69,40,94,2,58)),List(List(49,2,55,54,61),List(48,19,34,5,83),List(80,52,67,24,96),List(51,91,20,45,68),List(87,79,59,9,3)),List(List(47,12,71,88,74),List(28,5,79,58,26),List(93,67,62,86,23),List(66,13,96,46,17),List(94,59,19,54,15)),List(List(21,89,98,54,53),List(49,44,79,10,93),List(64,24,25,9,56),List(57,70,55,65,23),List(14,36,31,13,4)),List(List(62,60,30,89,94),List(88,19,59,41,75),List(25,45,74,17,47),List(5,16,76,33,58),List(53,68,65,39,67)),List(List(55,2,76,32,26),List(37,25,5,27,24),List(61,88,33,45,46),List(20,96,51,42,49),List(66,3,15,11,36)),List(List(60,21,80,9,96),List(91,39,24,28,13),List(52,11,34,41,82),List(66,85,72,38,76),List(69,25,67,64,81)),List(List(67,75,42,79,74),List(36,26,85,30,25),List(50,19,3,33,28),List(12,95,54,71,91),List(0,17,87,92,40)),List(List(51,85,12,86,40),List(28,36,35,50,97),List(55,16,20,14,73),List(7,5,4,68,22),List(47,3,67,93,2)),List(List(48,33,92,35,31),List(73,40,71,75,62),List(19,54,49,20,38),List(23,37,9,11,10),List(80,63,39,52,56)),List(List(59,70,61,65,62),List(42,73,99,39,66),List(67,8,93,30,97),List(53,37,51,55,11),List(48,26,94,44,63)),List(List(99,5,21,8,13),List(0,35,25,19,6),List(93,83,40,98,43),List(84,18,66,50,62),List(86,94,32,52,11)),List(List(55,15,85,39,4),List(95,83,27,46,45),List(19,47,61,9,66),List(82,32,72,77,16),List(50,96,14,60,35)),List(List(66,13,84,74,97),List(85,67,20,43,34),List(95,0,3,58,38),List(48,69,93,28,7),List(91,98,56,94,35)),List(List(11,15,73,51,77),List(13,7,22,53,10),List(2,40,98,79,50),List(71,83,49,45,56),List(0,1,68,99,24)),List(List(34,84,37,31,93),List(55,7,18,15,65),List(80,40,29,44,36),List(51,26,99,59,2),List(57,45,67,1,41)),List(List(79,90,56,76,58),List(78,70,20,26,48),List(87,82,46,59,98),List(51,81,91,52,44),List(21,86,68,64,7)),List(List(12,26,73,30,87),List(99,58,45,25,38),List(95,97,27,22,37),List(98,72,10,6,79),List(4,61,20,85,67)),List(List(9,26,5,68,2),List(97,4,31,11,69),List(75,64,0,6,17),List(25,95,89,59,38),List(16,99,27,53,10)),List(List(89,71,42,70,90),List(12,38,8,63,23),List(95,77,0,29,43),List(81,93,56,2,34),List(46,44,55,13,41)),List(List(72,21,50,1,81),List(67,44,88,90,82),List(98,19,30,48,85),List(66,20,79,13,28),List(29,62,38,74,89)),List(List(1,62,20,28,0),List(59,52,11,6,74),List(32,16,50,34,76),List(79,91,31,24,56),List(26,37,87,53,57)),List(List(47,79,55,45,9),List(63,2,1,60,75),List(18,39,97,7,44),List(33,29,91,31,23),List(50,80,32,49,71)),List(List(41,52,85,2,83),List(28,27,49,14,44),List(20,1,34,19,17),List(62,59,68,86,82),List(89,31,37,95,80)),List(List(67,70,59,17,91),List(3,60,12,6,93),List(99,44,34,9,21),List(31,26,61,20,25),List(23,15,43,53,42)),List(List(52,19,16,91,35),List(65,29,4,2,48),List(90,44,77,38,60),List(49,62,53,47,74),List(61,15,30,28,70)),List(List(14,97,34,88,55),List(50,28,80,36,64),List(93,40,60,90,22),List(29,77,1,26,56),List(33,9,4,67,68)))
val theDraws = List(74,79,46,2,19,27,31,90,21,83,94,77,0,29,38,72,42,23,6,62,45,95,41,55,93,69,39,17,12,1,20,53,49,71,61,13,88,25,87,26,50,58,28,51,89,64,3,80,36,65,57,92,52,86,98,78,9,33,44,63,16,34,97,60,40,66,75,4,7,84,22,43,11,85,91,32,48,14,18,76,8,47,24,81,35,30,82,67,37,70,15,5,73,59,54,68,56,96,99,10)

  

