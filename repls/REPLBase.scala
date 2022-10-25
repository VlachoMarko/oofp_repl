package repls

import repls.IntREPL.{isNumber, isVariable}

import scala.collection.mutable

/*
    The parent class of IntREPL and MultiSetREPL.
 */
abstract class REPLBase extends REPL {
    // This Base type is to make a generic REPL. In the Readme you can find more details on how this can be used.
    type Base <: RingWithMinus[Base]

    def applyOperator(lhs: Base, op: String, rhs: Base): Base = {
        op match {
            case "+" => lhs + rhs // works now!
            case "-" => lhs - rhs // works now!
            case "*" => lhs * rhs // works now!
        }
    }

    abstract class Expression[Base <: RingWithMinus[Base]] {
        var value: String

        var binding: Map[String, Base]

        def describe: String =
            "The value of " + toString + " is " + value
    }


        case class Constant(n: Base) extends Expression[Base] {
            override var value: String = n.toString

            override var binding: Map[String, Base] = Map[String, Base](value -> n)

        }

        case class Negative(n: Expression[Base]) extends Expression[Base] {
            override var value: String = n.binding(n.value).toString

            override var binding: Map[String, Base] = Map[String, Base](value -> n.binding(n.value))
        }

        case class Variable(n: String) extends Expression[Base] {
            override var value: String = n
            override var binding: Map[String, Base] = Map[String, Base]()
        }

        case class Operator(lhs: Expression[Base], operator: String, rhs: Expression[Base]) extends Expression[Base] {

            override var binding: Map[String, Base] = Map[String, Base]()
            override var value: String = ""
            setValues(lhs, operator, rhs)

            private def setValues(lhs: Expression[Base], operator: String, rhs: Expression[Base]): Unit = {
                if (isStrValue(lhs, rhs)) {
                    println("strval")
                    //TODO: Value string should only be set
                    value = getStrVal(lhs, operator, rhs)
                } else {
                    println("intval")
                    //TODO: Here the value string should be set parallel
                    val tempVal = applyOperator(lhs.binding(lhs.value), operator, rhs.binding(rhs.value))
                    value = tempVal.toString
                    this.binding += (value -> tempVal)

                }
            }

            private def isStrValue(lhs: Expression[Base], rhs: Expression[Base]): Boolean = {
                isVar(lhs) || isVar(rhs)
            }

            private def isVar(exp: Expression[Base]): Boolean = {
                exp match {
                    case Operator(a, op, b) => {
                        isVar(a) || isVar(b)
                    }
                    case Variable(_) => true
                    case _ => false
                }
            }


            private def getStrVal(lhs: Expression[Base], op: String, rhs: Expression[Base]): String = {

                var lhsStr: String = getSideVal(lhs)
                var rhsStr: String = getSideVal(rhs)

                lhsStr + " " + op + " " + rhsStr
            }

            private def getSideVal(side: Expression[Base]): String = {
                side match {
                    case Operator(l, op, r) => {
                        if (isVar(side)) {
                            if (op == "*") {
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

}

object REPLBase {
    def isOp(str: String): Boolean = {
        str match {
            case "+" => true
            case "*" => true
            case "-" => true
            case _ => false
        }
    }

    def isNegative(str: String): Boolean = {
        if (str.charAt(0) != '-') return false
        val tempStr = str.slice(1, str.length)

        for (item <- tempStr.toCharArray) {
            if (!item.isDigit) return false
        }
        true
    }
}


