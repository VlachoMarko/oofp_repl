package repls

import repls.IntREPL.{getOpVal, getOperator, isElement, output}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IntREPL extends REPLBase {
    // Have a REPL of type Int
    type Base = Int
    override val replName: String = "ints" // TODO: name me!

    override def readEval(command: String): String = {
        val elements: Array[String] = command.split("\\s") // split string based on whitespace
        println(elements.mkString("Array(", ", ", ")"))
        RPN(elements)
        ""
    }

    def RPN(elements: Array[String]): Unit = {
        var rpn: mutable.Queue[String] = mutable.Queue[String]()
        val storedNumbers, operators: mutable.Stack[String] = mutable.Stack[String]()

        for (i <- elements.indices) {
            println(" Current: " + elements(i))
            if (isElement(elements(i))) {
                println("Push to rpn")
                rpn += elements(i)
            }
            else if (getOperator(elements(i)) != "0") {
                println("To ops")
                handleOps(elements(i))
            }
            else println("Cannot process element: " + elements(i))
            println("rpn: " + rpn + " | storedNumbers: " + storedNumbers + " | operators: " + operators)
        }

        for (i <- rpn.indices) {
            storedNumbers.push(rpn(i))
        }

        while (operators.nonEmpty) {
            if (operators.top == "(") {
                operators.pop()
            }
            else storedNumbers.push(operators.pop())

        }

        //pops rpn
        output(storedNumbers)

        def handleOps(op: String): Unit = {
            if (operators.nonEmpty && operators.top != "(") {

                if (op == '('.toString) {
                    println("Parentheses: (")
                    for (i <- rpn.indices) {
                        storedNumbers.push(rpn(i))
                    }
                    rpn = mutable.Queue[String]()
                    operators.push(op)

                }
                else if (op == ")") {
                    println("Parentheses: )")

                    for (i <- rpn.indices) {
                        storedNumbers.push(rpn(i))
                    }
                    while (operators.top != "(") {
                        storedNumbers.push(operators.pop())
                    }
                    operators.pop()
                    if (operators.nonEmpty) storedNumbers.push(operators.pop())

                    rpn = mutable.Queue[String]()

                }
                else {
                    println("Not parentheses")
                    val opVal: Int = getOpVal(op)
                    val stackVal: Int = getOpVal(operators.top)

                    if (opVal < stackVal) {
                        rpn += operators.pop()
                        operators.push(op)
                    }
                    else if (opVal == stackVal) {
                        if (operators.top == '-'.toString) {
                            rpn += operators.pop()
                            operators.push(op)
                        }
                        else operators.push(op)
                    }
                    else {
                        println("Push to inside ops"); operators.push(op)
                    }
                }
            }
            else {
                println("Push to ops"); operators.push(op)
            }
        }


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
            case _ => "0"
        }
    }

    def getOpVal(op: String): Int = {
        op match {
            case "+" => 2
            case "-" => 2
            case "*" => 3
        }
    }

    def isElement(str: String): Boolean = {
        if (isNumber(str) || getOperator(str) == "0") return true
        false
    }

    def isNumber(str: String): Boolean = {
        if (str.isEmpty) return false
        for (char <- str.toCharArray) {
            if (!char.isDigit) return false
        }
        true
    }

    def output(rpn: mutable.Stack[String]): Unit = {
        var printArr: mutable.ArrayBuffer[String] = ArrayBuffer[String]()

        while (rpn.nonEmpty) {
            printArr += rpn.pop()
        }
        printArr = printArr.reverse
        println(printArr.mkString("", " ", ""))
    }


}