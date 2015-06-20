package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength
import Polynomial.computeDelta
import Polynomial.computeSolutions

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("cyclic with two expressions") {
    val input: Map[String, Signal[Expr]] = Map(
      "a" -> Signal { Plus(Literal(3), Ref("b")) },
      "b" -> Signal { Plus(Ref("a"), Literal(1)) })

    val results = Calculator.computeValues(input)

    for ((k,result) <- results.mapValues(x => x()))
      assert(result.isNaN())
  }

  test("cyclic with four expressions") {
    val input: Map[String, Signal[Expr]] = Map(
      "a" -> Signal { Ref("c") },
      "b" -> Signal { Ref("b") },
      "c" -> Signal { Ref("a") },
      "d" -> Signal { Plus(Ref("a"), Literal(6))})

    val results = Calculator.computeValues(input)

    for ((k,result) <- results.mapValues(x => x()))
      assert(result.isNaN())
  }

  test("Square solution with delta equals zero") {
    val a = Signal(1.0)
    val b = Signal(4.0)
    val c = Signal(4.0)
    val results = computeSolutions(a, b, c, computeDelta(a, b, c))
    assert(results().size == 1)
  }

  test("Square solution with negative delta") {
    val a = Signal(4.0)
    val b = Signal(4.0)
    val c = Signal(4.0)
    val results = computeSolutions(a, b, c, computeDelta(a, b, c))
    assert(results().size == 0)
  }
}
