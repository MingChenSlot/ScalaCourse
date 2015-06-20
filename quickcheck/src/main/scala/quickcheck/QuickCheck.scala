package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  def insertListToHeap(xs: List[Int]): H = {
    if (xs.isEmpty) empty
    else insert(xs.head, insertListToHeap(xs.tail))
  }

  def sortHeap (h: H): List[Int] = {
    if(isEmpty(h)) List[Int]()
    else List(findMin(h)) ++ sortHeap(deleteMin(h))
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll {(a: Int, b: Int) =>
    val min = Math.min(a, b)
    val h = insert(b, insert(a, empty))
    findMin(h) == min
  }

  property("listmin") = forAll { l: List[Int] =>
    if (l.nonEmpty)
      findMin(insertListToHeap(l)) == l.min
    else
      true
  }

  property("listsort") = forAll {l: List[Int] =>
    val heapList = sortHeap(insertListToHeap(l))
    l.sorted.zip(heapList).filter(x => x._1 != x._2).size == 0
  }

  property("empty1") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    val h3 = meld(h1, h2)
    val m3 = if (isEmpty(h3)) 0 else findMin(h3)
    m3 == Math.min(m1, m2)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    heap <- oneOf[H](empty, genHeap)
  } yield insert(k, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
