package repls

trait RingWithMinus[T] {
  def +(rhs: T): T
  def *(rhs: T): T
  def -(rhs: T): T

}



