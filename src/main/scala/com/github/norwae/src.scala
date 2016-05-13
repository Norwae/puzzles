package com.github.norwae

sealed trait Positive[+A] {
  def flatMap[B: Numeric](op: A => Positive[B]): Positive[B]
  def map[B: Numeric](op: A => B): Positive[B]
  def filter(pred: A => Boolean): Positive[A]
  def foreach(block: A => Unit)
}

object ConstraintViolated extends Positive[Nothing] {
  def flatMap[B: Numeric](op: Nothing => Positive[B]) = this
  def map[B: Numeric](op: Nothing => B) = this
  def filter(pred: Nothing => Boolean) = this
  def foreach(block: Nothing => Unit) = ()
}

case  class Existing[A: Numeric] (value: A) extends Positive[A] {
  private val x = implicitly[Numeric[A]]

  def foreach(block: A => Unit) = block(value)

  def flatMap[B: Numeric](op: A => Positive[B]): Positive[B] = {
    op(value)
  }

  def map[B: Numeric](op: A => B): Positive[B] = {
    val y = implicitly[Numeric[B]]
    val result = op(value)
    if (y.lt(result, y.zero)) ConstraintViolated
    else new Existing(result)
  }

  def filter(filter: A => Boolean): Positive[A] = if (filter(value)) this else ConstraintViolated
}

object Positive {
  def apply[T](value: T)(implicit evidence: Numeric[T]) = {
    if (evidence.lt(value, evidence.zero)) ConstraintViolated else new Existing(value)
  }
}