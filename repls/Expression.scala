package repls

import repls.Expression.{isNegative, simplify}
import repls.IntREPL.{isNumber, isVariable}

import scala.collection.mutable


abstract class Expression {
  def value : String
  var binding : Map[String, Int]
  def describe : String =
    "The value of " + toString + " is " + value
}

case class Constant(n : Int) extends Expression {
  override val value: String = n.toString
  override var binding: Map[String, Int] = Map[String,Int]({value -> n})

}

case class Negative(n: Expression) extends Expression {
  override def value: String = n.binding(n.value).toString
  override var binding: Map[String, Int] = Map[String, Int]({value -> n.binding(n.value)})

  println("Neg binding: " + binding + " key: " + value)
}

 case class Variable(n: String) extends Expression {
   override def value: String = n
   override var binding: Map[String, Int] = Map[String,Int]()
}

case class Operator(lhs : Expression, operator : String, rhs : Expression ) extends Expression {
  override def value: String = getValue(lhs, operator, rhs)
  override var binding: Map[String, Int] = Map[String,Int]()
  if (!isStrValue(lhs, rhs)) binding += (value -> value.toInt)


  private def getValue(lhs: Expression, operator: String, rhs: Expression): String = {
    if (isStrValue(lhs, rhs)) {
      lhs.value + " " + operator + " " + rhs.value
    } else {
      println("int value")
      getIntValue(lhs.binding(lhs.value), operator, rhs.binding(rhs.value)).toString
    }
  }

  private def isStrValue(lhs: Expression, rhs: Expression) : Boolean = {
   isVar(lhs) || isVar(rhs)
  }

  private def isVar(exp: Expression): Boolean = {
    println("exp: " + exp)
    exp match {
      case Operator(a, op, b) => { isVar(a) || isVar(b) }
      case Variable(_) => {println("isVar true"); true}
      case _ => {println("isVar false"); false}
    }
  }

  private def getIntValue(lhs: Int, opName: String, rhs: Int): Int = {
    opName match {
      case "+" => lhs + rhs
      case "*" => lhs * rhs
      case "-" => lhs - rhs
    }
  }

  override def toString: String = "(" + lhs.toString + operator + rhs.toString + ")"
}

object Expression {

  def getExpression(input : String) : Expression = {
    var expressions : mutable.Stack[Expression] = mutable.Stack[Expression]()
    var rpn : String = input
    var simple : Boolean = false

    if (input.charAt(0) == '@'){
      rpn = rpn.slice(2, rpn.length)
      simple = true
    }

    println("final rpn: " + rpn)

    for(el : String <- rpn.split(" ")){

      if (isOp(el) && simple){
        val rhs = expressions.pop()
        val lhs = expressions.pop()
        expressions.push(simplify(Operator(lhs, el, rhs)))
      }
      else if (isOp(el)) {
        val rhs = expressions.pop()
        val lhs = expressions.pop()
        val result = Operator(lhs, el, rhs)
        expressions.push(result)
      }
      else if (isNegative(el)) {
        expressions.push(Negative(Constant(el.toInt)))
      }
      else if (isVariable(el)){
        expressions.push(Variable(el))
      }
      else if (isNumber(el)){
        expressions.push(Constant(el.toInt))
      } else throw new Error("Unknown expression element " + el)

      if (simple){
        expressions.push(simplify(expressions.pop()))
      }

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

  def simplify(exp: Expression) : Expression = {

    exp match {
      case Negative(Negative(n)) => Negative(n)
      case Negative(e) => Negative(simplify(e))

      case Operator(e, "-", Constant(0)) => simplify(e)
      case Operator(e, "+", Constant(0)) => simplify(e)
      case Operator(Constant(0), "+", e) => simplify(e)

      case Operator(e, "*", Constant(0)) => Constant(0)
      case Operator(Constant(0), "*", e) => Constant(0)

      case Operator(Variable(n), "*", Constant(0)) => Constant(0)
      case Operator(Constant(0), "*", Variable(n)) => Constant(0)

      case Operator(e, "*", Constant(1)) => simplify(e)
      case Operator(Constant(1), "*", e) => simplify(e)

      case Operator(l, "-", r) => if(l == r) {Constant(0)} else {Operator(simplify(l), "-", simplify(r))}
      case Operator(l, op, r) => Operator(simplify(l), op, simplify(r))
      case _ => exp
    }
  }



}

