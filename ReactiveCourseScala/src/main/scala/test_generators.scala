/**
 * Created by mingchen on 4/18/15.
 */
import generators.Generator;

object test_generators extends App {
  def test[T](g: Generator[T], numTimes: Int = 100)
             (test: T => Boolean): Unit = {
    for (i <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "test failed for "+value)
    }
    println("passed " + numTimes +  " tests")
  }

}
