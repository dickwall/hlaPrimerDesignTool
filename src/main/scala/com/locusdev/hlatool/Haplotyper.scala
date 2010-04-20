package com.locusdev.hlatool

import collection.mutable.ListBuffer

/**
 * Identify and optimize haplotypes
 */

class Haplotyper {

  /**
   * Used to ensure matching length on any two sequences or lists that need to be the same length to be zipped.
   * Note: structural typing used - as long as the object answers to a length property, we can use it
   * @param lengthyThing1 an instance some type that has a length property
   * @param lengthyThing2 another instance of some type that has a length property
   * @throws IllegalArgumentException if the instances have different length
   *
   */
  def ensureMatchingLength(lengthyThing1: {def length: Int}, lengthyThing2: {def length: Int}) =
    if (lengthyThing1.length != lengthyThing2.length)
      throw new IllegalArgumentException("This method will not handle sequences of differing lengths")


  /**
   * Extract the mutation sets for a pair of sequences. The mutation sets are an array of sets of the possible
   * mutations at any given point in the sequence. For many positions, this will be a single possible nucleotide
   * but where mutations are indicated in the sequences, there will be more than one nucleotide in the set.
   * @param seq1 a string sequence
   * @param seq2 a second string sequence to compare to the first
   * @return an array of set is  of chars with the possible nucleotides at each position in the sequence
   */
  def extractMutationSets(seq1: String, seq2: String): Array[Set[Char]] = {
    ensureMatchingLength(seq1, seq2)
    val zipped = seq1.toArray zip seq2.toArray
    zipped map {case (allele1, allele2) => Set(allele1, allele2)}
  }

  /**
   * Given mutation sets obtained from either comparing sequence strings, or combining other mutation sets,
   * merge two mutation sets together. This can be used as a big reduce function over an entire list of mutation sets.
   * @param mset1 mutation set one (an array of sets of chars representing posssible nucleotides)
   * @param mset2 as for mset1, the mutation set that will be merged in
   * @return a new array of sets of chars with the possible nucleotides at each position in the sequence
   */
  def combineMutationSets(mset1: Array[Set[Char]], mset2: Array[Set[Char]]) = {
    ensureMatchingLength(mset1, mset2)
    val zipped = mset1 zip mset2
    zipped map {case (set1, set2) => set1 ++ set2}
  }

  /**
   * For a list of sequence strings, first find all of the mutation sets by comparing all strings with a pair, then
   * reduce the resulting list of sets by combining them to create new outer sets of possibilities for each location.
   * @param sequences, the list of all possible sequences for a region of interest
   * @return an array of sets of characters for each position in the sequence, along with the possible nucleotides there
   */
  def findAllMutations(sequences: List[String]): Array[Set[Char]] = {
    val (first: List[String], second: List[String]) = {
      if (sequences.length % 2 == 0) (sequences.splitAt(sequences.length / 2))
      else {
        val firstItem = sequences.head
        val rest = sequences.tail
        val (first, second) = rest.splitAt(rest.length / 2)
        (List(firstItem) ::: first, List(firstItem) ::: second)
      }
    }
    val zipped = first zip second
    val pairedMutations = zipped map {case (s1, s2) => extractMutationSets(s1, s2)}

    if (pairedMutations == Nil) Array()
    else pairedMutations reduceLeft {combineMutationSets(_, _)}
  }

  /**
   * Given an array of possible nucleotides for each position in a sequence (the output from findAllMutations), return
   * a map of the index of any position in that sequence that has observed mutations, along with the possible
   * nucleotides. Positions with only one observed nucleotide will be excluded from the map.
   * @param mutationList the Array of sets of nucleotides at each position in a sequence
   * @return a map of the index of any locations with more than one possible nucleotide, to a set of nucleotides
   * that can be at that position.
   */
  def mutationMap(mutationList: Array[Set[Char]]) = {
    var mmap = Map[Int, Set[Char]]()

    for (i <- 0 to (mutationList.length - 1)) {
      if (mutationList(i).size > 1) mmap += i -> mutationList(i)
    }

    mmap
  }

  /**
   * For a given sequence, along with an established mutation map for the nucleotides, return a list of Mutations
   * for the position index number and the nucleotide in this specific sequence for the possible mutation locations.
   */
  def getHaplotype(sequence: String, mutationMap: Map[Int, Set[Char]]) = {
    val chArray = sequence.toArray
    val haplotypeList = new ListBuffer[Mutation]

    for ((posn, possibles) <- mutationMap) {
      // check that the nucleotide for the sequence is valid
      val nucleotideFromSeq = chArray(posn)
      if (!possibles.contains(nucleotideFromSeq))
        throw new IllegalStateException("Unknown nucleotide %s for position %d".format(nucleotideFromSeq, posn))
      if (nucleotideFromSeq != Haplotyper.redactedMutation) {
        haplotypeList += Mutation(posn, nucleotideFromSeq)
      }
    }

    haplotypeList.readOnly
  }

  def eliminateHaplotype(mutation: Mutation, rest: List[String]) = {
    val posn = mutation.index
    val nucleotide = mutation.nucleotide

    rest filter {sequence => sequence.charAt(posn) == nucleotide}
  }

  def findLocalMinimum(handledMutations: List[Mutation], remainingMutations: List[Mutation],
                       rest: List[String], signatures: ListBuffer[List[Mutation]]): ListBuffer[List[Mutation]] = {

    val remaining = eliminateHaplotype(handledMutations.head, rest)


    if (remaining.length > 0) {
      for (mutation <- remainingMutations) {
        findLocalMinimum(remainingMutations.head :: handledMutations, remainingMutations.tail, remaining, signatures)
      }
    }
    else {
      signatures.append(handledMutations)
      signatures
    }

  }


  def findAllUniqueSignatures(sequence: String, rest: List[String], mutationMap: Map[Int, Set[Char]]) = {
    if (rest.contains(sequence)) throw new IllegalStateException("Sequence and comparison list must be exclusive")

    val itHap = getHaplotype(sequence, mutationMap)

    val signatures = new ListBuffer[List[Mutation]]

    // run through full list of possible positions
    for (mutation <- itHap) {
      val restOfList = itHap - mutation
      findLocalMinimum(List(mutation), restOfList, rest).sort((m1, m2) => (m1.index < m2.index), signatures)
    }

    val uniques = Set[List[Mutation]]() ++ signatures
    val uniqueList = List[List[Mutation]]() ++ uniques
    uniqueList.sort((l1, l2) => l1.length < l2.length)
  }

}

/**
 * A lightweight holder for sequence mutation details - the index of the possible mutation, and the specific
 * nucleotide at that location. Should be extended to include locus name, etc. in the future
 */
case class Mutation(val index: Int, val nucleotide: Char)

object Haplotyper {
  /**
   * In the process of building a consensus sequence between sequences that are, for our intents and purposes, the same,
   * certain mutations will be redacted.
   */
  val redactedMutation = '&'
}
