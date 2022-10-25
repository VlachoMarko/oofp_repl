/*
package repls

import repls.Expression.{isNegative, simplify}
import repls.IntREPL.{isNumber, isVariable}

import scala.collection.mutable

abstract class Expression[Base] extends REPLBase {

  def value : String
  var binding : Map[String, expBase]
  def describe : String =
    "The value of " + toString + " is " + value
}

case class Constant[expBase](n : expBase) extends Expression {
  override def value: String = n.toString
  override var binding: Map[String, Base] = Map[String, Base](value -> n)

}

case class Negative[Base](n: Expression[Base]) extends Expression[Base] {
  override def value: String = n.binding(n.value).toString
  override var binding: Map[String, Base] = Map[String, Base](value -> n)
}

 case class Variable[Base](n: String) extends Expression[Base] {
   override var value: String = n
   override var binding: Map[String, Base] = Map[String, Base]()
}

case class Operator[Base](lhs : Expression[Base], operator : String, rhs : Expression[Base]) extends Expression[Base]{

  override var binding: Map[String, Base] = Map[String, Base]()
  override var value: String = " "
  setValues(lhs, operator, rhs)

   private def setValues(lhs: Expression[Base], operator: String, rhs: Expression[Base]): Unit = {
    if (isStrValue(lhs, rhs)) {
      println("strval")
      //TODO: Value string should only be set
      value = getStrVal(lhs, operator, rhs)
    } else {
      println("intval")
      //TODO: Here the value string should be set parallel
      var tempVal = applyOperator(lhs.binding(lhs.value), operator, rhs.binding(rhs.value))
      this.binding += (value -> tempVal)

    }
  }

  private def isStrValue[Base](lhs: Expression[Base], rhs: Expression[Base]) : Boolean = {
   isVar(lhs) || isVar(rhs)
  }

  private def isVar[Base](exp: Expression[Base]): Boolean = {
    exp match {
      case Operator(a, op, b) => { isVar(a) || isVar(b) }
      case Variable(_) => true
      case _ => false
    }
  }


  private def getStrVal[Base](lhs: Expression[Base], op: String, rhs: Expression[Base]) : String = {

    var lhsStr : String = getSideVal(lhs)
    var rhsStr : String = getSideVal(rhs)

    lhsStr + " " + op + " " + rhsStr
  }

  private def getSideVal(side: Expression[Base]) : String = {
    side match {
      case Operator(l, op, r) => {
        if (isVar(side)) {
          if(op == "*") {
            getSideVal(l) + " " + op + " " + getSideVal(r)
          }
          else {
            "( " + getSideVal(l) + " " + op + " " + getSideVal(r) + " )"
          }
        }
        else {
          side.value
        }
      }
      case _ => side.value
    }
  }

  override def toString: String = "(" + lhs.toString + operator + rhs.toString + ")"


}

object Expression {

  def getExpression[T](input : String) : Expression[T] = {
    var expressions : mutable.Stack[Expression[T]] = mutable.Stack[Expression[T]()
    var rpn : String = input
    var simple : Boolean = false

    if (input.charAt(0) == '@'){
      rpn = rpn.slice(2, rpn.length)
      simple = true
    }

    for(el : String <- rpn.split(" ")){
      println("curr el: " + el)
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
        expressions.push(Negative(Constant(el)))
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

      // println("added: " + expressions.top)
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

  def simplify[T](exp: Expression[T]) : Expression[T] = {

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

      case Operator(Operator(l1, "*", r1), "+", Operator(l2, "*", r2)) => distributivity(l1, l2, r1, r2)
      case Operator(l, "-", r) => {
        if(l == r) {
          Constant(0)
        } else {Operator(simplify(l), "-", simplify(r))}
      }
      case Operator(l, op, r) => Operator(simplify(l), op, simplify(r))
      case _ => exp
    }
  }

  private def distributivity[T](l1: Expression[T], r1 : Expression[T], l2: Expression[T], r2: Expression[T]) : Expression[T] = {
    if (l1 == l2) {
      Operator(simplify(l1), "*", Operator(simplify(r1), "+", simplify(r2)))
    }
    else if (l1 == r2) {
      Operator(simplify(l1), "*", Operator(simplify(r1), "+", simplify(l2)))
    }
    else if (r1 == r2){
      Operator(simplify(r1), "*", Operator(simplify(l1), "+", simplify(l2)))
    }
    else if (r1 == l2){
      Operator(simplify(r1), "*", Operator(simplify(l1), "+", simplify(r2)))
    }
    else {
      // println("dont know what to do")
      Operator(Operator(l1, "*", r1), "+", Operator(l2, "*", r2))
    }


  }


}

*/
