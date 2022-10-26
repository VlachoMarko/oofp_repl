package repls

import repls.REPLBase.{getOpVal, getOperator, isElement}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/*
    The parent class of IntREPL and MultiSetREPL.
 */
abstract class REPLBase extends REPL {
    // This Base type is to make a generic REPL. In the Readme you can find more details on how this can be used.
    type Base <: RingWithMinus[Base]
    var variables : Map[String, Base]

    def applyOperator(lhs: Base, op: String, rhs: Base): Base = {
        op match {
            case "+" => lhs + rhs // works now!
            case "-" => lhs - rhs // works now!
            case "*" => lhs * rhs // works now!
        }
    }

    def getRPN(elements: Array[String]): mutable.Stack[String] = {
        println(elements.mkString(" "))
        var tempElements: mutable.Queue[String] = mutable.Queue[String]()
        val operators: mutable.Stack[String] = mutable.Stack[String]()
        val rpn: mutable.Stack[String] = mutable.Stack[String]()


        for (i <- elements.indices) {
            val item = elements(i)
            if (variables.contains(item)) {
                println("variables contains: " + variables(item))
                tempElements += variables(item).toString.filter(_ != ',')
            }
            else if (isElement(item)) {
                tempElements += item
            }
            else if (getOperator(item) != "notOp") {
                // println("To op")
                handleOps(item)
            }
            else println("Cannot process element: " + elements(i))

            // println("Loop | temp: " + tempElements + " | rpn: " + rpn + " | ops: " + operators.mkString("Stack:( ", ", ", " )"))
        }

        for (i <- tempElements.indices) {
            rpn.push(tempElements(i))
        }

        while (operators.nonEmpty) {
            if (operators.top == "(" || operators.top == ")") {
                operators.pop()
            }
            else rpn.push(operators.pop())
        }

        def handleOps(op: String): Unit = {
            if (operators.nonEmpty) {

                if (op == '('.toString) {

                    for (i <- tempElements.indices) {
                        rpn.push(tempElements(i))
                    }
                    tempElements = mutable.Queue[String]()
                    operators.push(op)
                }
                else if (op == ')'.toString) {

                    for (i <- tempElements.indices) {
                        rpn.push(tempElements(i))
                    }
                    while (operators.top != "(" && operators.top != ")") {
                        rpn.push(operators.pop())
                    }
                    operators.pop()
                    tempElements = mutable.Queue[String]()
                }
                else if (operators.top != "(") {

                    val opVal: Int = getOpVal(op)
                    val stackVal: Int = getOpVal(operators.top)

                    if (opVal < stackVal) {
                        tempElements += operators.pop()
                        operators.push(op)
                    }
                    else if (opVal == stackVal) {
                        if (operators.top == '-'.toString) {
                            tempElements += operators.pop()
                            operators.push(op)
                        } else operators.push(op)

                    } else {
                        operators.push(op)
                    }

                } else {
                    operators.push(op)
                }

            } else {
                operators.push(op)
            }
        }
        rpn
    }

    abstract class Expression[Base <: RingWithMinus[Base]] {
        var value: String

        var binding: Map[String, Base]
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
                    println("opVal")
                    //TODO: Here the value string should be set parallel
                    val tempBase = applyOperator(lhs.binding(lhs.value), operator, rhs.binding(rhs.value))
                    println("tempBase: " + tempBase)
                    value = tempBase.toString
                    this.binding += (value -> tempBase)

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

    def stackToString(rpn: mutable.Stack[String]): String = {
        var printArr: mutable.ArrayBuffer[String] = ArrayBuffer[String]()
        var printStr: String = ""

        while (rpn.nonEmpty) {
            printArr += rpn.pop()
        }
        printArr = printArr.reverse

        for (i <- printArr.indices) {
            printStr += printArr(i)
            printStr += " "
        }
        println("rpn as string: " + printStr)
        printStr
    }

    def getOperator(str: String): String = {
        str match {
            case "+" => "+"
            case "-" => "-"
            case "*" => "*"
            case "(" => "("
            case ")" => ")"
            case _ => "notOp"
        }
    }

    def getOpVal(op: String): Int = {
        op match {
            case "+" => 2
            case "-" => 2
            case "*" => 3
            case _ => 0
        }
    }

    def isElement(str: String): Boolean = {
        if (isNumber(str) || getOperator(str) == "notOp") return true
        false
    }

    def isVariable(str: String): Boolean = {
        if (!isNumber(str) && getOperator(str) == "notOp") return true
        false
    }

    def isNumber(str: String): Boolean = {
        if (str.isEmpty) return false

        for(item <- str.toCharArray){
            if (!item.isDigit) return false
        }
        true
    }
}


