package com.locusdev.hlatool

import collection.mutable.{HashSet, ListBuffer}
import collection.immutable.Set
import scala.annotation.tailrec

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
  def extractMutationSets(seq1: String, seq2: String): IndexedSeq[Set[Char]] = {
    ensureMatchingLength(seq1, seq2)
    val zipped = seq1 zip seq2
    zipped map {case (allele1, allele2) => Set(allele1, allele2)}
  }

  /**
   * Given mutation sets obtained from either comparing sequence strings, or combining other mutation sets,
   * merge two mutation sets together. This can be used as a big reduce function over an entire list of mutation sets.
   * @param mset1 mutation set one (an array of sets of chars representing posssible nucleotides)
   * @param mset2 as for mset1, the mutation set that will be merged in
   * @return a new array of sets of chars with the possible nucleotides at each position in the sequence
   */
  def combineMutationSets(mset1: IndexedSeq[Set[Char]], mset2: IndexedSeq[Set[Char]]) = {
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
  def findAllMutations(sequences: List[String]): IndexedSeq[Set[Char]] = {
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

    if (pairedMutations == Nil) IndexedSeq()
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
  def mutationMap(mutationList: IndexedSeq[Set[Char]]) = {
    var mmap = Map[Int, Set[Char]]()

    for (i <- 0 to (mutationList.length - 1)) {
      //filter out redacted mutations
      val mutations: Set[Char] = mutationList(i) - '*'
      if (mutations.size > 1) mmap += i -> mutations
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

  
  def eliminateHaplotype(mutation: Mutation, rest: List[String]) = 
      rest filter {sequence => sequence.charAt(mutation.index) == mutation.nucleotide}

      
  def findLocalMinimum(handledMutations: List[Mutation], remainingMutations: List[Mutation],
                       rest: List[String], signatures: HashSet[List[Mutation]], howDeep: Int): HashSet[List[Mutation]] = {

    println(howDeep)
    if (howDeep <= Haplotyper.maxChainLength) {

      val remaining = eliminateHaplotype(handledMutations.head, rest)

      println(handledMutations.length + ", " + remainingMutations.length);

      if (remaining.length > 0) {
        for (mutation <- remainingMutations) {
          findLocalMinimum(mutation :: handledMutations, remainingMutations.filterNot(_ == mutation), remaining, signatures, howDeep + 1)
        }
      }
      else {
        signatures.addEntry(handledMutations.sortWith((m1, m2) => (m1.index < m2.index)))
      }
    }

    signatures
  }


  def findAllUniqueSignatures(sequence: String, rest: List[String], mutationMap: Map[Int, Set[Char]]) = {
    if (rest.contains(sequence)) throw new IllegalStateException("Sequence and comparison list must be exclusive")

    val itHap = getHaplotype(sequence, mutationMap)

    val signatures = new HashSet[List[Mutation]]

    // run through full list of possible positions
    for (mutation <- itHap) {
      println("iteration")
      val restOfList = itHap.filterNot(_ == mutation)
      findLocalMinimum(List(mutation), restOfList, rest, signatures, 0)
    }

    val uniqueList = List[List[Mutation]]() ++ signatures
    uniqueList.sortWith((l1, l2) => l1.length < l2.length)
  }

  def findAnswer(data : scala.collection.Map[String, scala.collection.Map[String, String]], mutationMap : Map[Int, Set[Char]], allelePrefix : String) = {
    // partition by matching prefix or not
    // first, convert the map to a list of Allele case class instances
    val allelesList = data.map { case(k,v) => Allele(k, v) }
    // split into included and excluded alleles, by matching the allele prefix
    val (includedAlleles, excludedAlleles) = allelesList.partition( allele => allele.name.startsWith(allelePrefix))
    // sort the indices of the mutation map numerically by key
    val sortedIndices = mutationMap.keys.toList.sorted

    // if there aren't any alleles included - we can go no further
    if (includedAlleles.size == 0) throw new IllegalStateException("No matching alleles for " + allelePrefix)

    val firstAllele = includedAlleles.head

    val filteredIndices = sortedIndices filter { index =>
      // don't include if the allele is a base sequence (identified as "*")
      if (!(firstAllele baseSequenced index)) false
      else {
         !includedAlleles.tail.exists { otherAllele => firstAllele.sequence.charAt(index) != otherAllele.sequence.charAt(index) }
      }
    }

    val commonMutationsForIncluded = filteredIndices.map { index => Mutation(index, firstAllele.sequence.charAt(index)) }

    val matchesOthers = excludedAlleles filter { otherAllele => otherAllele.matches(commonMutationsForIncluded) }
    if (!(matchesOthers.isEmpty)) throw new IllegalStateException("Common mutations for group " + allelePrefix + " also matches " + matchesOthers)

    greedyGuess(commonMutationsForIncluded, excludedAlleles.toList)
  }


  @tailrec
  final def greedyGuess(mutations : List[Mutation], excludedAlleles : List[Allele], runningAnswer : List[Mutation] = Nil) : List[Mutation] = {

    excludedAlleles.size match {
      case 0 => runningAnswer
      case _ => {
        val resorted = sortByMutationFrequencies(mutations, excludedAlleles)
        val answer = resorted.head :: runningAnswer
        greedyGuess(resorted.tail, eliminateAlleles(answer, excludedAlleles), answer)
      }
    }

  }

  def eliminateAlleles(mutations : List[Mutation], alleles : List[Allele]) =
    alleles filter { _ matches mutations }

  def sortByMutationFrequencies(mutations : List[Mutation], alleles : List[Allele]) = {
    val mutationsWithFrequency = mutations map { mutation =>
      (mutation, (alleles filter { allele => allele.matches(mutation) }).size)
    }

    val sortedMutationsWithFrequency = mutationsWithFrequency.sortBy { _._2 }

    println(sortedMutationsWithFrequency.slice(0,10))

    sortedMutationsWithFrequency map { _._1 }
  }
}

/**
 * A lightweight holder for sequence mutation details - the index of the possible mutation, and the specific
 * nucleotide at that location. Should be extended to include locus name, etc. in the future
 */
case class Mutation(val index: Int, val nucleotide: Char){
  override def toString() = "[" + index + ":" + nucleotide + "]"
}

object Haplotyper {
  /**
   * In the process of building a consensus sequence between sequences that are, for our intents and purposes, the same,
   * certain mutations will be redacted.
   */
  val redactedMutation = '*'

  val maxChainLength = 5;

  def combineBlocks(blocks: scala.collection.Map[String, String]) = {
    val builder = new StringBuilder
    blocks.keys.toList.sorted.foreach(builder + blocks(_))
    builder.toString
  }
}
