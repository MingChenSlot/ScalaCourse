package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("concatRecovered given test case 1") {
    val requestStream = Observable.from(1 to 5)
    def requestMethod(num: Int) = if (num != 4) Observable.just(num) else Observable.error(new Exception)
    val actual = requestStream.concatRecovered(requestMethod).toBlocking.toList
    assert(actual.toString == "List(Success(1), Success(2), Success(3), Failure(java.lang.Exception), Success(5))")
  }

  test("concatRecovered given test case 2") {
    val request = Observable.just(1, 2, 3).concatRecovered(num => Observable.just(num, num, num))
    val expectedString = "List(Success(1), Success(1), Success(1), Success(2), Success(2), Success(2), Success(3), Success(3), Success(3))"
    val actual = request.toBlocking.toList.toString
    println(actual)
    assert(actual == expectedString, actual)
  }

  test("WikipediaApi should correctly use concatRecovered, concatenating many responses, even with eexceptions") {
    val requests = Observable.just(1, 2, 3)
    val remoteComputation = (num: Int) =>
      if (num != 2) Observable.just(num, num, num)
      else Observable.just(num) ++ Observable.error(new Exception) ++ Observable.just(num)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => acc // Failures are 0
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 1 + 2 + 3 + 3 + 3), s"Sum: $total")
  }

  def delay( dur: Duration) = {
    try {
      Await.result(Promise().future, dur)
    } catch {
      case ex: TimeoutException =>
    }
  }

  test("Timing out after 5 seconds") {
    val orig = Observable.interval(1 second).take(10)
    val toSubscribeTo: Observable[Long] = orig.timedOut(5)
    var total: Long = 0
    toSubscribeTo.subscribe {
      s => total += s
    }
    delay( 8 seconds)
    assert(total == 6, "Only 0,1,2,3 (adding up to 6) should come out in 5 seconds")
  }

  test("Observable should complete before timeout") {
    val start = System.currentTimeMillis
    val timedOutStream = Observable.from(1 to 3).zip(Observable.interval(100 millis)).timedOut(3L)
    val contents = timedOutStream.toBlocking.toList
    val totalTime = System.currentTimeMillis - start
    assert(contents == List((1,0),(2,1),(3,2)))
    assert(totalTime <= 1000)
  }

  test("Observable(1, 2, 3).zip(Observable.interval(400 millis)).timedOut(1L) should return the first two values, and complete without errors") {
    val timedOutStream = Observable.from(1 to 3).zip(Observable.interval(400 millis)).timedOut(1L)
    val contents = timedOutStream.toBlocking.toList
    assert(contents == List((1,0),(2,1)))
  }

  test("Observable(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1L) should return the first value, and complete without errors") {
    val timedOutStream = Observable.from(1 to 3).zip(Observable.interval(700 millis)).timedOut(1L)
    val contents = timedOutStream.toBlocking.toList
    assert(contents == List((1,0)))
  }

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable.just("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }
  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable.just(1, 2, 3)
    val remoteComputation = (n: Int) => Observable.just(0 to n : _*)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

}
