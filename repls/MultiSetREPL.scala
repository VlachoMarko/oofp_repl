package repls

import repls.REPLBase.{isOp, isVariable, stackToString}

import scala.collection.mutable

class MultiSetREPL extends REPLBase {
    // Have a REPL of a MultiSet of strings
    override type Base = MultiSet[String]
    override val replName: String = "multiset" // TODO: name me!
    var variables : Map[String, MultiSet[String]] = Map[String, MultiSet[String]]()



    override def readEval(command: String): String = {
        println("ms first: " + command)
        var elements : Array[String] = getElements(command)

        println("elements: " + elements.mkString)

        if (elements.length > 1 && isVariable(elements(0)) && elements(1) == "=") {
            val key: String = elements(0)
            elements = elements.slice(2, elements.length)
            val exp: Expression[Base] = getExpression(stackToString(getRPN(elements)))
            variables += (key -> exp.binding(exp.value))

            key + " = " + variables(key).toString
        }
        else {
            val exp: Expression[Base] = getExpression(stackToString(getRPN(elements)))
            exp.value
        }

    }

    def getExpression(elements: String): Expression[Base] = {

        var expressions: mutable.Stack[Expression[Base]] = mutable.Stack[Expression[Base]]()
        var mElements : Seq[String] = Seq[String]()
        var simple: Boolean = false
        var multiset : Boolean = false

       /* if (input.charAt(0) == '@') {
            elements = elements.slice(2, elements.length)
            simple = true
        }*/

        for (el <- elements.split(" ")) {

            println("curr el: " + el)
            if (isOp(el)) {
               val rhs = expressions.pop()
                val lhs = expressions.pop()
                expressions.push(Operator(lhs, el, rhs))
                println("op")
            }
            else if (el.charAt(0) == '{'){

                for(i <- el.indices){
                    if(el(i) != '{' && el(i) != '}'){
                        mElements = mElements :+ el(i).toString
                    }
                }
                expressions.push(Constant(MultiSet[String](mElements)))
                mElements = Seq[String]()
                println("added expression: " + expressions.top.value)
            }

            // println("added: " + expressions.top)
        }

         expressions.top
        }

    private def getElements(command: String): Array[String] = {
        var result : Array[String] = Array[String]()
        var tempStr : String = ""
        var multiset : Boolean = false

        for(i <- command.indices){
            val el = command(i)
            if (el.isWhitespace || el == ',') {}
            else if (el == '{') {
                multiset = true
                tempStr += el.toString
            }
            else if (el == '}') {
                multiset = false
                tempStr += el.toString
                result = result :+ tempStr
                // println("added: " + tempStr)
                tempStr = ""
            }
            else if (multiset && el != ',') {
                // println("before: " + tempStr)
                tempStr += el.toString
                // println("after: " + tempStr)
            }
            else if(isOp(el.toString) || el == '(' || el == ')' || isVariable(el.toString) || el == '='){
                // println("before: " + tempStr)
                result = result :+ el.toString
                // println("after: " + tempStr)
            }

        }

        /*for(el <- result.indices){
            println("el" + el + ": " + result(el))
        }*/

        result
    }
}

object MultiSetREPL {

}