package repls
import repls.IntREPL.isVariable
import repls.MultiSet

class MultiSetREPL extends REPLBase {
    // Have a REPL of a MultiSet of strings
    override type Base = MultiSet[String]
    override val replName: String = "multiset" // TODO: name me!
    var variables : Map[String, MultiSet[String]] = Map[String, MultiSet[String]]()

    override def readEval(command: String): String = {
        val elements: Array[String] = command.split("\\s") // split string based on whitespace
        println(elements.mkString(" "))

        if (elements.length > 1 && isVariable(elements(0)) && (elements(1) == '='.toString)) {
            val key: String = elements(0)
            // ("sliced: " + elements.slice(2, elements.length).mkString("Array(", ", ", ")"))

            // variables += (key -> getValue(elements.slice(2, elements.length)).toInt)

            // println(key + " -> " + variables(elements(0)))
            // key + " = " + variables(key).toString

        } else {

        }
        ""
    }

    override def applyOperator(lhs: MultiSet[String], op: String, rhs: MultiSet[String]): MultiSet[String] = super.applyOperator(lhs, op, rhs)
}

