package repls

import repls.MultiSet.empty

/*
    Multiset is a Map of elements and their respective count.
    For example:
    {a,a,a,b,c,c} = Map('a'->3, 'b'->1, 'c'->2)
 */


case class MultiSet[T] (multiplicity: Map[T, Int], elements : Seq[T] = Seq[T]()) {


    /* TODO
        Intersection of two multisets:
        ∀x m_c(x) = min(m_a(x), m_b(x))
        Example:
        {a,b,b,c,c,c} * {b,c,c,c,c} = {b,c,c,c}
     */
    def *(that: MultiSet[T]): MultiSet[T] = empty[T]

    /* TODO
        Summation of two multisets:
        ∀x m_c(x) = m_a(x) + m_b(x)
        Example:
        {a,b,c,c} + {a,c,d} = {a,a,b,c,c,c,d}
     */
    def +(that: MultiSet[T]): MultiSet[T] = {

        var resElements : Seq[T] = Seq[T]()

        val elements1 : Seq[T] = this.elements
        val elements2 : Seq[T] = that.elements

        for(el <- elements1){
            resElements = resElements :+ el
        }
        for(el <- elements2){
            resElements = resElements :+ el
        }
        println("res length: " + resElements.length)

        MultiSet[T](resElements)
    }

    /* TODO
        Subtraction of two multisets:
        ∀x m_c(x) = max(m_a(x) - m_b(x), 0)
        Example:
        {a,b,b,d} - {b,c,c,d,d} = {a,b}
     */
    def -(that: MultiSet[T]): MultiSet[T] = empty[T]
    /* TODO
        Make sure a multiset can be returned as a sequence.

        For example the multiset {a,a,b} should give the sequence Seq(a,a,b).

        The order of the elements in the sequence does not matter.
     */
    def toSeq: Seq[T] = {
        elements
    }

    val MaxCountForDuplicatePrint = 5

    // A toString has already been provided
    override def toString: String = {
        def elemToString(elem : T) : String = {
            val count = multiplicity(elem)
            if(count >= MaxCountForDuplicatePrint)
                elem.toString + " -> " + count.toString
            else Seq.fill(count)(elem).mkString(",")
        }
        val keyStringSet = multiplicity.keySet.map(elemToString)
        "{" + keyStringSet.toSeq.sorted.mkString(",") + "}"
    }


}

object MultiSet {
    def empty[T] : MultiSet[T] = MultiSet(Map[T,Int]())


    /* TODO
        Write a constructor that constructs a multiset from a sequence of elements
     */
    def apply[T](elements: Seq[T]): MultiSet[T] = {
        var multiplicity : Map[T, Int] = Map[T, Int]()

        elements.foreach(addToSet)

        def addToSet(el: T): Unit = {

            if (multiplicity.contains(el)) {
                multiplicity.updated(el, multiplicity(el) + 1)
                // println("contains, increased: " + el)
            }
            else {
                multiplicity += (el -> 1)
                // println("not contains, added: " + el)
            }
        }
        MultiSet[T](multiplicity, elements)
    }

    //TODO: getElements(multiplicities)


}
