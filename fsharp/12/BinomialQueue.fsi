namespace HillClimbing.MinPriorityQueue

    module BinomialQueue =
        type BinomialQueue<'E when 'E : comparison>

        /// <summary>Creates a new empty priority queue.</summary>
        val empty<'E >  : BinomialQueue<'E> when 'E : comparison
        /// <summary>Returns wether the queue is empty or not.</summary>
        val isEmpty     : BinomialQueue<'E> -> bool when 'E : comparison
        /// <summary>Returns a new priority queue with the element inserted.</summary>
        val insert      : 'E -> BinomialQueue<'E> -> BinomialQueue<'E> when 'E : comparison
        /// <summary>Merges two priority queues into a new priority queue</summary>
        val meld        : BinomialQueue<'E> -> BinomialQueue<'E> -> BinomialQueue<'E> when 'E : comparison
        /// <summary>Exception indicating that the priority queue is empty.</summary>
        exception Empty
        /// <summary>Returns the element of the lowest priority in the queue</summary>
        /// <exception cref="Empty">Thrown when the queue is empty.</exception>
        val findMin     : BinomialQueue<'E> -> 'E when 'E : comparison
        /// <summary>Returns a new priority queue with the element of the lowest priority removed.</summary>
        /// <exception cref="Empty">Thrown when the queue is empty.</exception>
        val deleteMin   : BinomialQueue<'E> -> BinomialQueue<'E> when 'E : comparison
