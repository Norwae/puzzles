package com.github.norwae

sealed trait Positive[+A] {
  // Your code here
}

object ConstraintViolated extends Positive[Nothing] {
  // Your code here
}

case  class Existing[A: Numeric] (value: A) extends Positive[A] {
  private val x = implicitly[Numeric[A]]

  // Your code here
}

object Positive {
  def apply[T](value: T)(implicit evidence: Numeric[T]) = {
    if (evidence.lt(value, evidence.zero)) ConstraintViolated else Existing(value)
  }
}