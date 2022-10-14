package repls

import repls.IntREPL.{isElement, isNumber}

import scala.collection.mutable


abstract class Expression {
  def value : Int
  def describe : String =
    "The value of " + toString + " is " + value.toString
}

case class Constant(n : Int) extends Expression {
  override def value: Int = n

  override def toString: String = n.toString
}

case class Operator(lhs : Expression, operatorName : String, rhs : Expression ) extends Expression {
  override def value: Int = getOpValue(lhs.value, operatorName, rhs.value)

  private def getOpValue(lhs: Int, opName: String, rhs: Int): Int = {
    opName match {
      case "+" => lhs + rhs
      case "*" => lhs * rhs
      case "-" => lhs - rhs
    }
  }

  override def toString: String = "(" + lhs.toString + operatorName + rhs.toString + ")"
}

object Expression {

  def getExpression(rpn : String) : Expression = {
    var expressions : mutable.Stack[Expression] = mutable.Stack[Expression]()

    println("final rpn: " + rpn)

    for(el : String <- rpn.split(" ")){
      if (isOp(el)){
        val rhs = expressions.pop()
        val lhs = expressions.pop()
        val result = Operator(lhs, el, rhs)
        expressions.push(result)
      }
      else if (isNumber(el) || isElement(el)){
        expressions.push(Constant(el.toInt))
      } else throw new Error("Unknown expression element " + el)

      println("value: " + expressions.top.value)
    }

    expressions.top
  }

  private def isOp(str: String): Boolean = {
    str match {
      case "+" => true
      case "*" => true
      case "-" => true
      case _ => false
    }
  }
}

