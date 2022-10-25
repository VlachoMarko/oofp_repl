package repls

import repls.IntREPL.{getOpVal, getOperator, isElement, isNumber, isVariable, stackToString}
import repls.REPLBase.{isNegative, isOp}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IntREPL extends REPLBase {
    // Have a REPL of type Int
    override type Base = IntWrap

    override val replName: String = "ints" // TODO: name me!
    var variables : Map[String, Base] = Map[String, Base]()

    override def readEval(command: String): String = {
        val elements: Array[String] = command.split("\\s") // split string based on whitespace
        // println(elements.mkString(" "))



        if (isVariable(elements(0)) && elements(1) == '='.toString) {
            val key : String = elements(0)
            val exp : Expression[Base] = getExp(elements.slice(2, elements.length))
            variables += (key -> exp.binding(exp.value))

            key + " = " + variables(key).toString
        }
        else {
            val exp : Expression[Base] = getExp(elements)
            exp.value
        }

    }

    def getExp(elements: Array[String]): Expression[Base] = {
        var simpleMode : Boolean = if (elements(0) == "@") true else false

        val exp : Expression[Base] = getIntExpression(stackToString(getRPN(elements)))

        if (simpleMode) {
            println("@final: " + exp.value)
            exp
        }
        else {
            exp
        }

    }

    def getRPN(elements: Array[String]): mutable.Stack[String] = {
        var tempElements: mutable.Queue[String] = mutable.Queue[String]()
        val operators: mutable.Stack[String] = mutable.Stack[String]()
        val rpn: mutable.Stack[String] = mutable.Stack[String]()


        for (i <- elements.indices) {

            if (variables.contains(elements(i))){
                // println("toTemp: " + variables(elements(i)))
                tempElements += variables(elements(i)).toString
            }
            else if (isElement(elements(i))) {
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

    def getIntExpression(input: String): Expression[Base] = {
        var expressions: mutable.Stack[Expression[Base]] = mutable.Stack[Expression[Base]]()
        var rpn: String = input
        var simple: Boolean = false

        if (input.charAt(0) == '@') {
            rpn = rpn.slice(2, rpn.length)
            simple = true
        }

        for (el: String <- rpn.split(" ")) {
            println("curr el: " + el)
            if (isOp(el) && simple) {
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
                expressions.push(Negative(Constant(IntWrap(el.toInt))))
            }
            else if (isVariable(el)) {
                expressions.push(Variable(el))
            }
            else if (isNumber(el)) {
                expressions.push(Constant(IntWrap(el.toInt)))
            } else throw new Error("Unknown expression element " + el)

            if (simple) {
                expressions.push(simplify(expressions.pop()))
            }

            // println("added: " + expressions.top)
        }

        expressions.top
    }

    def simplify(exp: Expression[Base]): Expression[Base] = {

        exp match {
            case Negative(Negative(n)) => Negative(n)
            case Negative(e) => Negative(simplify(e))

            case Operator(e, "-", Constant(IntWrap(0))) => simplify(e)
            case Operator(e, "+", Constant(IntWrap(0))) => simplify(e)
            case Operator(Constant(IntWrap(0)), "+", e) => simplify(e)

            case Operator(e, "*", Constant(IntWrap(0))) => Constant(IntWrap(0))
            case Operator(Constant(IntWrap(0)), "*", e) => Constant(IntWrap(0))

            case Operator(Variable(n), "*", Constant(IntWrap(0))) => Constant(IntWrap(0))
            case Operator(Constant(IntWrap(0)), "*", Variable(n)) => Constant(IntWrap(0))

            case Operator(e, "*", Constant(IntWrap(1))) => simplify(e)
            case Operator(Constant(IntWrap(1)), "*", e) => simplify(e)

            case Operator(Operator(l1, "*", r1), "+", Operator(l2, "*", r2)) => distributivity(l1, l2, r1, r2)
            case Operator(l, "-", r) => {
                if (l == r) {
                    Constant(IntWrap(0))
                } else {
                    Operator(simplify(l), "-", simplify(r))
                }
            }
            case Operator(l, op, r) => Operator(simplify(l), op, simplify(r))
            case _ => exp
        }
    }

    def distributivity(l1: Expression[Base], r1: Expression[Base], l2: Expression[Base], r2: Expression[Base]): Expression[Base] = {
        if (l1 == l2) {
            Operator(simplify(l1), "*", Operator(simplify(r1), "+", simplify(r2)))
        }
        else if (l1 == r2) {
            Operator(simplify(l1), "*", Operator(simplify(r1), "+", simplify(l2)))
        }
        else if (r1 == r2) {
            Operator(simplify(r1), "*", Operator(simplify(l1), "+", simplify(l2)))
        }
        else if (r1 == l2) {
            Operator(simplify(r1), "*", Operator(simplify(l1), "+", simplify(r2)))
        }
        else {
            // println("dont know what to do")
            Operator(Operator(l1, "*", r1), "+", Operator(l2, "*", r2))
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