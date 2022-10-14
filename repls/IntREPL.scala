package repls

import repls.Expression.getExpression
import repls.IntREPL.{getOpVal, getOperator, isElement, isVariable, stackToString}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IntREPL extends REPLBase {
    // Have a REPL of type Int
    type Base = Int
    override val replName: String = "ints" // TODO: name me!
    var variables : Map[String, Int] = Map[String, Int]()
    var value : Int = 0

    override def readEval(command: String): String = {
        val elements: Array[String] = command.split("\\s") // split string based on whitespace
        println(elements.mkString(" "))
        if (isVariable(elements(0)) && elements(1) == '='.toString) {
            val key : String = elements(0)
            println("sliced: " + elements.slice(2, elements.length).mkString("Array(", ", ", ")"))
            variables += (key -> getValue(elements.slice(2, elements.length)))
            println(key + " -> " + variables(elements(0)))

            key + " = " + variables(key).toString
        } else {
            getValue(elements).toString
        }

    }

    def getValue(elements: Array[String]): Int = {

        value = getExpression(stackToString(getRPN(elements))).value
        println("final: " + value.toString)
        value
    }

    def getRPN(elements: Array[String]): mutable.Stack[String] = {
        var tempElements: mutable.Queue[String] = mutable.Queue[String]()
        val operators: mutable.Stack[String] = mutable.Stack[String]()
        val rpn: mutable.Stack[String] = mutable.Stack[String]()


        for (i <- elements.indices) {
             // println(" Current: " + elements(i))
            if (isElement(elements(i))) {
               // println("Push el: " + elements(i))
               tempElements += elements(i)
            }
            else if (getOperator(elements(i)) != "notOp") {
                // println("To op")
                handleOps(elements(i))
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
                else if (operators.top != "("){

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

                    } else { operators.push(op) }

                } else { operators.push(op) }

            } else { operators.push(op) }
        }

        rpn
    }


}

object IntREPL {


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

    def isVariable(str: String) : Boolean = {
        if (!isNumber(str) && getOperator(str) == "notOp") return true
        false
    }

    def isNumber(str: String): Boolean = {
        if (str.isEmpty) return false
        for (item <- str.toCharArray) {
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

        for(i <- printArr.indices){
            printStr += printArr(i)
            printStr += " "
        }
        printStr
    }


}