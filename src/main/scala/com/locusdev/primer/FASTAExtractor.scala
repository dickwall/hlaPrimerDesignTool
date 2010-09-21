package com.locusdev.primer

import io.Source
import java.lang.String
import collection.Iterator
import java.io.File
import com.locusdev.model.Exon

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
    val exon = new Exon(args(6), args(1), args(2).toInt, args(3).toInt, None, None)
    println(P3Parser.createP3Input(chromosomeDir, exon, args(4).toInt, args(5).toInt))
  }


  def getFullChromosomeSequence(chromosomeDir: File, chromosome: String): String = {
    if (!chromosomeDir.exists || chromosomeDir.isFile) {
      throw new IllegalStateException(chromosomeDir.getAbsolutePath + " is not a directory")
    }

    //look for the fasta file
    val chromFile = new File(chromosomeDir, if (chromosome.startsWith("chr")) chromosome else {"chr" + chromosome} + ".fa")
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
    sequence
  }

  def extractSequence(chromosomeDir: File, chromosome: String, startPosition: Int, endPosition: Int): String = {
    extractSequence(getFullChromosomeSequence(chromosomeDir, chromosome), startPosition, endPosition)
  }

  def extractSequence(sequence: String, start: Int, end: Int) = {


    try {
      val extracted = sequence.substring(start, end)
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