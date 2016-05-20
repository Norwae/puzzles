package com.github.norwae

final class DIYCaseClass(val x: Int, val y: Int) extends Product with Serializable {
  def flip = copy(y, x)

  def copy(x: Int = this.x, y: Int = this.y) = DIYCaseClass(x, y)

  def canEqual(other: Any) = other.isInstanceOf[DIYCaseClass]
  override def equals(other: Any) = canEqual(other) && {
    val cast = other.asInstanceOf[DIYCaseClass]
    cast.x == x && cast.y == y
  }

  override def productElement(n: Int) = n match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException
  }

  override def productArity = 2
}

object DIYCaseClass extends ((Int, Int) => DIYCaseClass) {
  override def apply(x: Int, y: Int) = new DIYCaseClass(x, y)

  def unapply(arg: DIYCaseClass): Option[(Int, Int)] = Some((arg.x, arg.y))
}