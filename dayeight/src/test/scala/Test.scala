package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestDisplayOutput extends AnyFlatSpec with Matchers {
  val testinput: List[String] = List("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb","fdgacbe cefdb cefbgd gcbe","edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec","fcgedb cgb dgebacf gc","fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef","cg cg fdcagb cbg","fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega","efabcd cedba gadfec cb","aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga","gecf egdcabf bgf bfgea","fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf","gebdcfa ecba ca fadegcb","dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf","cefg dcbef fcge gbcadfe","bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd","ed bcgafe cdgba cbgef","egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg","gbdfcae bgc cg cgb","gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc","fgae cfgab fg bagce")
  val digitOutputValues = getTheParsedDigitOutputValue(testinput)
  digitOutputValues.length shouldEqual 40

  val amountOf1478 = getAmountOf1478(testinput)
  amountOf1478 shouldEqual 26

  def transformFromDecoder(input:String, decoder:Map[Char,Char]): String = {
    var decoded = ""
    for(char <- input) {
      val decodedChar = decoder.get(char).getOrElse('.')
      decoded += decodedChar
    }
    decoded
  }

  def getTheParsedDigitOutputValue(input: List[String]): List[String] = {
    val parsed = input.map(x => x.split("\\s"))
    parsed.filter(x => x.length == 4).flatten
  }

  def getAmountOf1478(input: List[String]): Int = {
    val parsedInput = getTheParsedDigitOutputValue(input)
    parsedInput.filter(x => (x.length == 2) || (x.length == 3) || (x.length == 4) || (x.length == 7)).length
  }
}
