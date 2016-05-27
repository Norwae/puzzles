package com.github.norwae

import org.scalatest.{FlatSpec, Matchers}

class AmazingSyntaxTest extends FlatSpec with Matchers {
  import Right._
  "Amazing scala syntax" should "be surprisingly flexible about left/right associativity" in {
    val l = Left(100)
    l <-: Right shouldEqual 100
  }

  it should "not be shy about switching things around for types either" in {
    val test: String :+: Int = ("Johnny", 22)

    test._1 shouldEqual "Johnny"
    test._2 shouldEqual 22
  }

  it should """allow "adding methods" to final classes?!""" in {
    val l = Left(1919)

    l.double shouldEqual Left(3838)
  }

  it should "infer inplace operators" in {
    var l = Left(10)
    l += Left(30)

    l shouldEqual Left(40)
  }

  it should "allow mutator overrides" in {
    val as = AmazingSyntax("abc", Nil)
    as.name = "Wow"

    as shouldEqual AmazingSyntax("WowWow", Nil)
  }

  it should "allow accessor overrides" in {
    val as = AmazingSyntax("FooBoo", List("17"))
    as.name shouldEqual "FooBoo!"
  }

  it should "allow an odd extractor syntax" in {
    val as = AmazingSyntax("hey", Nil)
    val as() = ()
    as.values = List("1", "2", "3")
    val as(one, two, three) = ()
    one shouldEqual "1"
    two shouldEqual "2"
    three shouldEqual "3"
  }

  it should "be surprisingly flexible about LValues" in {
    val as = AmazingSyntax("FooBoo", List("17"))
    as(0) = "Sabre"

    as shouldEqual AmazingSyntax("FooBoo", List("Sabre"))
  }

  it should "permit defining unary operators" in {
    val as = AmazingSyntax("FooBoo", List("17"))

    ~as shouldEqual "ooBooF"
  }
}
