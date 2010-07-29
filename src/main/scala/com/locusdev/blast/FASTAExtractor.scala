package com.locusdev.blast

import java.io.File
import io.Source
import java.lang.String
import collection.Iterator

/**
 * Created by IntelliJ IDEA.
 * User: alexfurman
 * Date: Jul 8, 2010
 * Time: 2:38:32 PM
 * To change this template use File | Settings | File Templates.
 */

object FASTAExtractor {
  def main(args: Array[String]) {

    if (args.size != 7) {
      //only print the usage when we don't have the right arguments 
      printUsage();
      System.exit(0)
    }

    //check that the chromosome dir exists
    val chromosomeDir = new File(args(0))
    if (!chromosomeDir.exists || chromosomeDir.isFile) {
      throw new IllegalStateException(chromosomeDir.getAbsolutePath + " is not a directory")
    }

    //look for the fasta file
    val chromFile = new File(chromosomeDir, if (args(1).startsWith("chr")) {args(1)} else {"chr" + args(1)} + ".fa")
    if (!chromFile.exists || chromFile.isDirectory) {
      throw new IllegalStateException(chromFile.getAbsolutePath + " is not a file")
    }

    //extract the FASTA string from chrom file (concatanate all non-comment lines into a single string of sequence)
    val lines: Iterator[String] = Source.fromFile(chromFile).getLines()

    val builder = new StringBuilder()



    while (lines.hasNext) {
      val line = lines.next
      if (!line.startsWith(">")) {
        builder.append(line)
      }
    }

    val sequence = builder.toString

    //now, extract the piece we need
    val extracted = extractSequence(sequence, args(2).toInt, args(3).toInt, args(4).toInt, args(5).toInt, args(6))

  }

  def extractSequence(sequence: String, startPosition: Int, endPosition: Int, margin: Int, size: Int, id: String) = {
    val targetLength: Int = endPosition - startPosition
    val start = startPosition - margin - size;
    val end = endPosition + margin + size;

    try {
      val extracted = sequence.substring(start, end)

      println("PRIMER_SEQUENCE_ID=" + id)
      println("SEQUENCE_TEMPLATE=" + extracted)
      println("SEQUENCE_TARGET=" + size + "," + (endPosition - startPosition + margin * 2))
      println("=")

      extracted
    }
    catch {
      case e => {
        System.err.println("start = " + start)
        System.err.println("end = " + end)
        System.err.println("chrom size = " + sequence.length)
        throw e
      }


    }
  }


  def printUsage() = {


    println("FASTAExtractor chromosome_dir chromosome start_position end_position margin size name")
    println("\tchromosome_dir\t-directory containing FASTA files for each chromosome (from UCSC Genome)")
    println("\tchromosome\t\t-chromosome to extract sequence from")
    println("\tstart_position\t-start position for the target sequence")
    println("\tend_position\t-end position for the target sequence")
    println("\tmargin\t\t\t-how many bases on each side should be added to the target sequence")
    println("\tsize\t\t\t-number of bases to each side of the target sequene (plus margin) to extract")
    println("\tname\t\t\t-primer sequence id to be passed to primer3")

    println()
    println("Will extract target sequence and (margin + size) base pairs around it from a UCSC FASTA file")
  }


}