package repls

import repls.Expression.isNegative
import repls.IntREPL.{isNumber, isVariable}

import scala.collection.mutable


abstract class Expression {
  def value : String
  var binding : Map[String, Int]
  def describe : String =
    "The value of " + toString + " is " + value
}

case class Constant(n : String) extends Expression {
  override val value: String = n
  override var binding: Map[String, Int] = Map[String,Int]({n -> n.toInt})

}

case class Negative(n: Int) extends Expression {
  override def value: String = (-n).toString
  override var binding: Map[String, Int] = Map[String, Int]({(-n).toString -> n})

  println("Neg binding: " + binding + " key: " + (-n).toString)
}

 case class Variable(n: String) extends Expression {
   override def value: String = n
   override var binding: Map[String, Int] = Map[String,Int]()
}

case class Operator(lhs : Expression, operatorName : String, rhs : Expression ) extends Expression {
  override def value: String = getValue(lhs, operatorName, rhs)
  override var binding: Map[String, Int] = Map[String,Int]()
  if (isNumber(value) || isNegative(value)) binding += (value -> value.toInt)

  private def getValue(lhs: Expression, operatorName: String, rhs: Expression): String = {
    if (isNumber(lhs.value) || isNegative(lhs.value) || isNumber(rhs.value) || isNegative(rhs.value)) {
      println("int value")
      getIntValue(lhs.binding(lhs.value), operatorName, rhs.binding(rhs.value)).toString
    }
    else if (isVariable(lhs.value) || isVariable(rhs.value)) {
      println("str value")
      getStrValue(lhs.value, operatorName, rhs.value)
    }
    else throw new Error("Can't handle this operation: " + lhs.value + " " + operatorName + " " + rhs.value)
  }

  private def getIntValue(lhs: Int, opName: String, rhs: Int): Int = {
    opName match {
      case "+" => lhs + rhs
      case "*" => lhs * rhs
      case "-" => lhs - rhs
    }
  }
  private def getStrValue(lhs: String, opName: String, rhs: String) : String = {
    opName match {
      case "+" => lhs + " + " +  rhs
      case "*" => lhs + " * " + rhs
      case "-" => lhs + " - " + rhs
    }
  }



  override def toString: String = "(" + lhs.toString + operatorName + rhs.toString + ")"
}

object Expression {

  def getExpression(input : String) : Expression = {
    var expressions : mutable.Stack[Expression] = mutable.Stack[Expression]()
    var rpn : String = input

    if (input.charAt(0) == '@'){
      rpn = rpn.slice(2, rpn.length)
    }

    println("final rpn: " + rpn)

    for(el : String <- rpn.split(" ")){
      if (isOp(el)){
        val rhs = expressions.pop()
        val lhs = expressions.pop()
        val result = Operator(lhs, el, rhs)
        expressions.push(result)
      }
      else if (isNegative(el)) {
        expressions.push(Negative(el.toInt))
      }
      else if (isVariable(el)){
        expressions.push(Variable(el))
      }
      else if (isNumber(el)){
        expressions.push(Constant(el))
      } else throw new Error("Unknown expression element " + el)

      println("added: " + expressions.top)
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

   def isNegative(str: String) : Boolean = {
    if (str.charAt(0) != '-') return false
    val tempStr = str.slice(1, str.length)

    for (item <- tempStr.toCharArray) {
      if (!item.isDigit) return false
    }
    true
  }

}

