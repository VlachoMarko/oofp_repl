package repls

import repls.MultiSet.{max, min}

/*
    Multiset is a Map of elements and their respective count.
    For example:
    {a,a,a,b,c,c} = Map('a'->3, 'b'->1, 'c'->2)
 */


case class MultiSet[T](multiplicity: Map[T, Int], elements : Seq[T] = Seq[T]()) extends RingWithMinus[MultiSet[T]]{



    /* TODO
        Intersection of two multisets:
        ∀x m_c(x) = min(m_a(x), m_b(x))
        Example:
        {a,b,b,c,c,c} * {b,c,c,c,c} = {b,c,c,c}
     */


    def *(that: MultiSet[T]): MultiSet[T] = {

        var resMap = emptyMap

        val mul1 = this.multiplicity
        val mul2 = that.multiplicity
        var resMul: Int = 0

        val keySet1 = this.multiplicity.keySet
        val keySet2 = that.multiplicity.keySet

        for (key <- keySet1) {
            if (keySet2.contains(key)) {
                resMul = min(mul1(key), mul2(key))

                resMap += (key -> resMul)
            }
        }

        resMap = clearZeroes(resMap)
        MultiSet[T](resMap)
    }

    /* TODO
        Summation of two multisets:
        ∀x m_c(x) = m_a(x) + m_b(x)
        Example:
        {a,b,c,c} + {a,c,d} = {a,a,b,c,c,c,d}
     */
    def +(that: MultiSet[T]): MultiSet[T] = {

        var resMap = emptyMap

        val mul1 = this.multiplicity
        val mul2 = that.multiplicity
        var resMul = 0

        val keys1 = mul1.keys
        val keys2 = mul2.keys

        val keySet1 = mul1.keySet
        val keySet2 = mul2.keySet

        for (key <- keys1) {
            if (keySet2.contains(key) && !resMap.contains(key)) {
                resMul = mul1(key) + mul2(key)
                resMap += (key -> resMul)
            }
            else if (!resMap.contains(key)) {
                resMap += (key -> mul1(key))
            }
        }

        for (key <- keys2) {
            if (!keySet1.contains(key) && !resMap.contains(key)) {
                resMap += (key -> mul2(key))
            }
        }

        resMap = clearZeroes(resMap)
        MultiSet[T](resMap)
    }

    /* TODO
        Subtraction of two multisets:
        ∀x m_c(x) = max(m_a(x) - m_b(x), 0)
        Example:
        {a,b,b,d} - {b,c,c,d,d} = {a,b}
     */
    def -(that: MultiSet[T]): MultiSet[T] = {

        var resMap = emptyMap

        val mul1 = this.multiplicity
        val mul2 = that.multiplicity
        var resMul = 0

        val keys1 = mul1.keys
        val keySet2 = mul2.keySet

        for (key <- keys1) {
            if(!keySet2.contains(key) && !resMap.contains(key)){
                resMap += (key -> mul1(key))
            }
            else if(!resMap.contains(key)){

                resMul = max(mul1(key) - mul2(key), 0)
                resMap += (key -> resMul)
            }

        }
        resMap = clearZeroes(resMap)
        MultiSet[T](resMap)
    }

    /* TODO
        Make sure a multiset can be returned as a sequence.

        For example the multiset {a,a,b} should give the sequence Seq(a,a,b).

        The order of the elements in the sequence does not matter.
     */
    def toSeq: Seq[T] = {
        var resElements: Seq[Seq[T]] = Seq[Seq[T]]()

        val keys = this.multiplicity.keys

        for(key <- keys){
            resElements = resElements :+ Seq.fill(this.multiplicity(key))(key)
        }

        resElements.flatten
    }

    private def emptyMap: Map[T, Int] = Map[T, Int]()

    val MaxCountForDuplicatePrint = 5

    // A toString has already been provided
    override def toString: String = {
        def elemToString(elem : T) : String = {
            val count = multiplicity(elem)
            if(count >= MaxCountForDuplicatePrint)
                elem.toString + " -> " + count.toString
            else Seq.fill(count)(elem).mkString(",")
        }
        println("mul: " + multiplicity)
        val keyStringSet = multiplicity.keySet.map(elemToString)
        "{" + keyStringSet.toSeq.sorted.mkString(",") + "}"


    }

    private def clearZeroes(mul: Map[T, Int]): Map[T, Int] = {
        var result: Map[T, Int] = Map[T, Int]()
        for (key <- mul.keys) {
            if (mul(key) != 0) {
                result += (key -> mul(key))
            }
        }
        result
    }
}

object MultiSet {
    def empty[T] : MultiSet[T] = MultiSet(Map[T,Int]())

    def apply[T](elements: Seq[T]): MultiSet[T] = {
        var multiplicity : Map[T, Int] = Map[T, Int]()

        elements.foreach(addToSet)

        def addToSet(el: T): Unit = {

            if (multiplicity.contains(el)) {
                var increase = 1
                increase += multiplicity(el)
                multiplicity = multiplicity + (el -> increase)
            }
            else {
                multiplicity += (el -> 1)
            }
        }
        MultiSet[T](multiplicity, elements)
    }

    def abs(number : Int) : Int = {
        if (number < 0) {number * (-1)}
        else number
    }

    def min(n1 : Int, n2 : Int) : Int = {
        if (n1 <= n2) n1 else n2
    }

    def max(n1: Int, n2: Int): Int = {
        if (n1 >= n2) n1 else n2
    }



}
