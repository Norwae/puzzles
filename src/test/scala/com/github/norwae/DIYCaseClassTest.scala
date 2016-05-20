package com.github.norwae

import org.scalatest.{FlatSpec, Matchers}

class DIYCaseClassTest extends FlatSpec with Matchers {
  "The DIY case class" should "be applicable"  in {
    val instance = DIYCaseClass(1, 2)
    instance.x shouldEqual 1
    instance.y shouldEqual 2
  }

  it should "offer a functioning copy function" in {
    val instance = new DIYCaseClass(1, 3).copy(x = 5).copy(y = 7)

    instance.x shouldEqual 5
    instance.y shouldEqual 7
  }

  it should "work as a function" in {
    val tuple = (7, 18)
    val created = DIYCaseClass.tupled.andThen(_.flip)(tuple)

    created.x shouldEqual 18
    created.y shouldEqual 7
  }

  it should "offer decompositions" in {
    val instance = new DIYCaseClass(9, 199)
    val DIYCaseClass(x, y) = instance

    x shouldEqual 9
    y shouldEqual 199

    instance match {
      case DIYCaseClass(`x`, anotherY) => anotherY shouldEqual y
    }
  }

  it should "be iterable" in {
    val instance = new DIYCaseClass(88, 171)
    instance.productIterator.toList should contain theSameElementsInOrderAs List(88, 171)
  }
}
