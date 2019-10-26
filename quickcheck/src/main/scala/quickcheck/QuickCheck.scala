package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  property("Insert 2 elements to an empty Heap, the minimum of the Heap equals the minimum of inserts") =
    forAll {(e1: A, e2 : A) =>
      val h: H = insert(e1, insert(e2, empty))
      findMin(h) == Math.min(e1, e2)
    }

  property("Deleting the minimum of 1-element Heap results an empty Heap") = forAll {
    e1: A => isEmpty(deleteMin(insert(e1, empty)))
  }

  property("Finding and deleting minimum can sort a Heap") = forAll {
    h: H =>
      def isSorted(h: H): Boolean = {
        if(isEmpty(h)) true
        else {
          val min = findMin(h)
          val tail = deleteMin(h)
          isEmpty(tail) || (min <= findMin(tail) && isSorted(tail))
        }
      }
      isSorted(h)
  }

  property("The minimum of melded Heap is the minimum of minimums") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("Compare two heaps equals") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("The minimum of consolidated heap is not changed") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = min(m1,m2)
    findMin(meld(deleteMin(h1), insert(m, h2))) == m
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}