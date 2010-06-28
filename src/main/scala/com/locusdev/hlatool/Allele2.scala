package com.locusdev.hlatool

/**
 * Created by IntelliJ IDEA.
 * User: alexfurman
 * Date: May 24, 2010
 * Time: 11:14:55 AM
 * To change this template use File | Settings | File Templates.
 */
class Allele2 private(val name: String, blocks: List[Block]) {
  /**
   * returns the full sequence for an Allele for use in the Haplotyper algorithm. This is done by concatenating the
   * individual sequences for each block
   */

  val sequence = blocks.map {_.sequence}.reduceLeft {_ + _}

  def baseSequenced(index: Int) = {
    if (sequence.length < index - 1) throw new IllegalArgumentException(this + " sequence out of bounds: " + index + ", length: " + sequence.length)
    sequence.charAt(index) != '*'
  }

  def matches(mutations: List[Mutation]): Boolean = {
    !(mutations exists {mutation => !matches(mutation)})
  }

  def matches(mutation: Mutation): Boolean = (sequence.charAt(mutation.index) == mutation.nucleotide)
}
class Block(val name: String, val sequence: String)

object Allele2 {
  def apply(name: String, blocks: List[Block]) = {
    new Allele2(name, blocks.sortBy {_.name})
  }
}

