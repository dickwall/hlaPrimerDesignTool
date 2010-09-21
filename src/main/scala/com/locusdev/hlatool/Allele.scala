package com.locusdev.hlatool

/**
 * Created by IntelliJ IDEA.
 * User: alexfurman
 * Date: May 18, 2010
 * Time: 1:28:35 PM
 * To change this template use File | Settings | File Templates.
 */

case class Allele(name : String, blocks : scala.collection.Map[String, String]) {

  def sequence = Haplotyper.combineBlocks(blocks)

  def baseSequenced(index : Int) = {
    if (sequence.length < index - 1) throw new IllegalArgumentException(this + " sequence out of bounds: " + index + ", length: " + sequence.length)
    sequence.charAt(index) != '*'
  }

  def matches(mutations : List[Mutation]) : Boolean = {
    !(mutations exists { mutation => !matches(mutation) })
  }

  

  def matches(mutation : Mutation) : Boolean = (sequence.charAt(mutation.index) == mutation.nucleotide)
}