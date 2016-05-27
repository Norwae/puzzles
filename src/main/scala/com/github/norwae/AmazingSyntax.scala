package com.github.norwae

case class AmazingSyntax(private var _name: String, var values: List[String]) {
  def update(idx: Int, v: String) = values = values.updated(idx, v)
  def unapplySeq(x: Any) = Some(values)
  def name_=(str: String): Unit = _name = str * 2
  def name = _name + "!"
  def unary_~ = _name.reverse
}

case class Left(value: Int) {
  def +(other: Left) = {
    Left(value + other.value)
  }
}
object Right {
  type :+:[A, B] = (A, B)
  def <-:(l: Left) = l.value

  implicit class RichLeft(l: Left) {
    def double = Left(l.value * 2)
  }
}