package repls

import repls.REPLBase.{isNegative, isNumber, isOp, isVariable, stackToString}
import scala.collection.mutable


class IntREPL extends REPLBase {
    // Have a REPL of type Int
    override type Base = IntWrap

    override val replName: String = "ints" // TODO: name me!
    var variables : Map[String, Base] = Map[String, Base]()

    override def readEval(command: String): String = {
        var elements: Array[String] = command.split("\\s") // split string based on whitespace
        // println(elements.mkString(" "))

        if (isVariable(elements(0)) && elements(1) == "=") {
            val key : String = elements(0)
            elements = elements.slice(2, elements.length)
            val exp : Expression[Base] = getIntExpression(stackToString(getRPN(elements)))
            variables += (key -> exp.binding(exp.value))

            key + " = " + variables(key).toString
        }
        else {
            val exp : Expression[Base] = getIntExpression(stackToString(getRPN(elements)))
            exp.value
        }

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
