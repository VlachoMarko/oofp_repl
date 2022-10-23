package repls

class MultiSetREPL extends REPLBase {
    // Have a REPL of a MultiSet of strings
    override type Base = MultiSet[String]
    override val replName: String = "multiset" // TODO: name me!

    override def readEval(command: String): String = {
        // TODO: complete me!
        println("multiset read eval" + command)
        ""
    }

    // TODO: Implement any further functions that are specifically for an MultiSetREPL
}
