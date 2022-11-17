package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = Gen.frequency(
    (1, Gen.const(empty)),
    (
      10,
      for {
        num <- Arbitrary.arbitrary[Int]
        heap <- Gen.frequency((1, Gen.const(empty)), (5, genHeap))
      } yield insert(num, heap)
    )
  )
  given Arbitrary[H] = Arbitrary(genHeap)

//  val sample = genHeap.sample

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minOf2") = Prop.forAll { (pair: (Int, Int)) =>
    val (a, b) = pair
    val minElement = findMin(insert(a, insert(b, empty)))
    if a < b then minElement == a else minElement == b
  }

  property("deleteMinFromHeapOf1") = Prop.forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("sortedSequence") = Prop.forAll { (heap: H) =>

    @tailrec
    def sortHeap(acc: Seq[Int], smallerHeap: H): Seq[Int] =
      if isEmpty(smallerHeap) then acc
      else sortHeap(acc :+ findMin(smallerHeap), deleteMin(smallerHeap))

    val maybeOrdered = sortHeap(Nil, heap)

    maybeOrdered == maybeOrdered.sorted
  }

  property("minOf2Heaps") = Prop.forAll { (pair: (H, H)) =>
    val (heap1, heap2) = pair
    if !isEmpty(heap1) && !isEmpty(heap2) then
      val min1 = findMin(heap1)
      val min2 = findMin(heap2)
      val globalMin = findMin(meld(heap1, heap2))

      if min1 < min2 then globalMin == min1 else globalMin == min2
    else true
  }

  property("findAllElementsOfOneHeapInMeld") = Prop.forAll { (pair: (H, H)) =>
    val (heap1, heap2) = pair

    @tailrec
    def sortHeap(acc: Seq[Int], smallerHeap: H): Seq[Int] =
      if isEmpty(smallerHeap) then acc
      else sortHeap(acc :+ findMin(smallerHeap), deleteMin(smallerHeap))

    val sortedHeap1 = sortHeap(Nil, heap1)
    val sortedHeap2 = sortHeap(Nil, heap2)

    val combinedHeap = meld(heap1, heap2)
    val sortedCombinedHeap = sortHeap(Nil, combinedHeap)

    sortedHeap1.forall(sortedCombinedHeap.contains) && sortedHeap2.forall(
      sortedCombinedHeap.contains
    )
  }
