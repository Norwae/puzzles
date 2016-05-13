package com.github.norwae

import org.scalatest.{FlatSpec, Matchers}

class Tests extends FlatSpec with Matchers {
  "The positive-numbers type" should "support simple for comprehensions" in {
    for (x <- Positive(3)) x shouldEqual 3
  }

  it should "support the for-if syntax" in {
    val result = for {
      x <- Positive(3)
      if x  < 20
    } yield x

    result shouldBe a[Existing[_]]
    result shouldEqual Positive(3)
  }

  it should "filter out negative values" in {
    val result = for {
      x <- Positive(3)
    } yield x - 10

    result shouldBe ConstraintViolated
  }
}
