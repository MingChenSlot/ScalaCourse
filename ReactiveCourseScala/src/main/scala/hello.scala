/**
 * Created by mingchen on 4/17/15.
 */
import generators._
import test_generators.test

object hello extends App{
  println("Functional testing")

  def sum(a: Int, b: Int) = a + b

  def sumf(f: (Int, Int) => Int)(a: Int, b: Int): Int = {
    f(a, a) + f(b, b)
  }

  def multiple(a: Int, b: Int) = a * b

  val test_val = sumf(multiple)(2, 5)

  println(test_val)

  val primes = List(1, 2, 3, 5, 7)
  val test_list = 1 to 10
  println(primes)

  def isPrime(num: Int): Boolean = primes.exists(num == _)

  // for list testing
  println("\n List testing")
  println(test_list.filter(isPrime(_)))
  println(test_list.map(isPrime(_)))

  // for comprehension testing
  println("\n for comprehension testing")
  var doubled = for (n <- 1 to 2 if isPrime(n)) yield n * 2
  println(doubled)

  def primePair(n:Int) =  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime (i + j)
  } yield (i, j)

  val for_xs = primePair(5)
  println(for_xs)

  val flat_xs = (1 until 5).flatMap(i => (1 until i).filter(j => isPrime(i + j)).map(j => (i, j)))
  println(flat_xs)

  println("\n Random Generator")
  println(choose(1, 16).generate)
  println(lists.generate)

  //tree generator
  println("\n Tree Generator")
  println(trees.generate)

  //test generator
  println("\n Test Generator")
  test(pairs(lists, lists), 100) {
    case (xs, ys) => (xs ++ ys).length >= xs.length
  }
}

