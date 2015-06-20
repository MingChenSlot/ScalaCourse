/**
 * Created by mingchen on 4/18/15.
 */
object generators extends App {
  // random generator
  trait Generator[T] {
    self =>
    def generate: T
    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }
    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  def integers = new Generator[Int] {
    val rand = new java.util.Random()
    def generate = rand.nextInt()
  }

  def booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)


  def single[T](x: T) = new Generator[T] {
    def generate = x
  }

  // chose one int in [lo, hi]
  def choose(lo: Int, hi: Int): Generator[Int] = {
    for (x <- integers) yield lo + Math.abs(x) % (hi - lo)
  }

  def oneOf[T](xs: T*): Generator[T] = {
    for (idx <- choose(0, xs.length)) yield xs(idx)
  }

  def emptyList = single(Nil)

  def nonEmptyLists = for {
    head <- choose(1,5)
    tail <- lists
  } yield (head::tail)

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyList else nonEmptyLists
  } yield list

  trait Tree
  case class Inner(left:Tree, right:Tree) extends Tree
  case class Leaf(x:Int) extends Tree

  def leaf = for {
    x <- integers
  } yield Leaf(x)

  def innerNode = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if(isLeaf) leaf else innerNode
  } yield tree
}
