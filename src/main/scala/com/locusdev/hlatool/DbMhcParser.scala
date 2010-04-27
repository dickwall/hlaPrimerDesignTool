package com.locusdev.hlatool

import xml.XML
import scala.collection._
import mutable.HashMap

/**
 *
 * The dbMHC database provides an open, publicly accessible platform for DNA and clinical data related to the
 * human Major Histocompatibility Complex
 *
 * http://www.ncbi.nlm.nih.gov/gv/mhc/main.cgi?cmd=init
 *
 * This is a simple parser/dataExtractor for the dbMHC XML format
 *
 * User: alexfurman
 * Date: Apr 8, 2010
 * Time: 11:21:40 AM
 */

object DbMhcParser {
  /**
   * Extracts and pre-processes sequence from the dbMHC xml file
   *
   * @param reader a reader into the dbMHC xml file
   * @param locus The locus we're interested in
   * @param dnaBlock the dna block (Exon1, Exon2, etc) we're interested in
   * @param relevantCharacters the number of characters that are relevant in the haplotype name. When there's more than
   * one haplotype that shares the same first characters in its name, the entries will be combined into one and the mutations
   * that are different between them will be redacted.  If there's nothing to redact, relevantCharacters should be passed in
   * as 0
   */
  def extractSequence(reader: java.io.Reader, locus: String, dnaBlock: Array[String], relevantCharacters: int) = {
    val start = System.currentTimeMillis;

    println("parsing dbHMC xml")
    val xml = XML load reader
    println("parsed dbHMC xml in " + ((System.currentTimeMillis - start) / 1000) + " seconds")

    val alleles = (xml \ "allele") filter {
      allele => (allele \ "locus" \ "name").text.equals(locus)
    }

    var data = immutable.Map[String, String]()

    for (allele <- alleles) {
      val name = (allele \ "name").text
      val blocks = allele \ "blocks" \ "block"
      for (block <- blocks) {
        val blockName = (block \ "name").text
        if (blockName.equals(dnaBlock)) {
          val sequence = (block \ "sequence").text
          //ensure sequence uniqueness
          data += name -> sequence
        }
      }
    }
    println("parsed " + data.size + " enries");

    redact(data, relevantCharacters)
  }

  def redact(data: Map[String, String], relevantCharacters: int) = {
    if (relevantCharacters == 0) {
      data
    }
    else {
      //group together all the alleles that share the same relevant characters in a name
      val grouped = new HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]

      data.foreach {entry => grouped add (entry._1.substring(0, relevantCharacters), entry._2)}
      //now, fold all the individual lists and redact all of the mutations that are not shared

      val redacted = new mutable.HashMap[String, String]
      val sequenceSet = new mutable.HashSet[String]

      grouped.foreach {
        entry =>
          val redactedSequence = entry._2.reduceLeft {
            (x: String, y: String) =>
              var consensus = ""

              for (i <- 0 to (x.length - 1)) {
                if (x.charAt(i) == y.charAt(i)) {
                  consensus += x.charAt(i)
                }
                else {
                  consensus += Haplotyper.redactedMutation
                }
              }
              consensus
          }

          if (!(sequenceSet contains redactedSequence)) {
            println(entry._1 + "=" + redactedSequence)
            redacted += entry._1 -> redactedSequence
          }
      }
      redacted
    }
  }
}