package com.locusdev.snp

import io.Source
import com.locusdev.primer.Primer
import java.io.{FileWriter, PrintWriter, File}

/**
 * Created by IntelliJ IDEA.
 * User: afurman
 * Date: Sep 15, 2010
 * Time: 12:57:27 PM
 */


object SnpSearch {
  def processPrimers(primerTable: File, snpFilesDir: File, output: PrintWriter) {
    println("Processing: " + primerTable.getAbsolutePath)
    //sanity check
    require(snpFilesDir.exists && snpFilesDir.isDirectory, snpFilesDir.getAbsolutePath + " is not a directory")
    require(primerTable.exists && primerTable.isFile, primerTable + " is not a file")

    val lines = Source.fromFile(primerTable).getLines
    //do we have a valid header
    val header = lines.next
    require(header startsWith "PRIMER_SEQUENCE\tPRIMER_START_POSITION\tPRIMER_PENALTY\tPRIMER_TM\tPRIMER_GC_PERCENT", "Unexpected header: " + header)
    output.println(header + "\tANY SHORT SNPS\tANY LONG SNPS\tREAL SHORT SNPS\tREAL LONG SNPS")

    //now, parse out the primers and
    //group the primers by chromosome. currently, each file will only contain primers on a single chromosome, but there's
    //no need to bake in that assumption
    val primers = lines.map(Primer(_)).toList.groupBy(_.chromosome)

    require(!primers.isEmpty, "The file does not contain any primers")

    val chromosomes = primers.keys.toList

    var count = 0

    chromosomes.foreach {
      chromosome =>
      //read out all the files for the chromosome
        val start = System.currentTimeMillis
        println("Parsing snps for chromosome " + chromosome)
        val anyShort = parseSnps(new File(snpFilesDir, "any_chr" + chromosome + "_short.txt")).toList
        val anyLong = parseSnps(new File(snpFilesDir, "any_chr" + chromosome + "_long.txt")).toList
        val realShort = parseSnps(new File(snpFilesDir, "real_chr" + chromosome + "_short.txt")).toList
        val realLong = parseSnps(new File(snpFilesDir, "real_chr" + chromosome + "_long.txt")).toList

        //now, actually go through all the primers and see if there's hits
        primers(chromosome).foreach{
          primer =>
          count += 1
          if(count % 100 == 0){
            println("Processed primers: " + count)
          }

          val anyShortHits = countHits(primer, anyShort)
          val anyLongHits = countHits(primer, anyLong)
          val realShortHits = countHits(primer, realShort)
          val realLongHits = countHits(primer, realLong)

          val filled = primer.setSnpHits(anyShortHits, anyLongHits, realShortHits, realLongHits)
          output.println(filled.toTableLine)
        }
      println("Took " + ((System.currentTimeMillis - start) / 60000) + " minutes" )
    }


  }

  def countHits(primer: Primer, snps: List[CondensedSnp]) = {
    var count = 0;

    snps.foreach(count += overlaps(_, primer))

    count

    //snps.map(overlaps(_,primer)).reduceLeft(_ + _)
  }

  def overlaps(snp: CondensedSnp, primer: Primer) = {
    if(
      //snp start is within primer
      (snp.start >= primer.startPosition && snp.start <= primer.endPosition) ||
      //snp end is within primer
      (snp.end >= primer.startPosition && snp.end <= primer.endPosition) ||
      //snp covers entire primer
      (snp.start < primer.startPosition && snp.end > primer.endPosition)
    ) 1 else 0
  }


  def parseSnps(file: File) = {
    require(file.exists, file.getAbsolutePath + " does not exist")
    Source.fromFile(file).getLines.map {
      line =>
        val tokens = line.split("\t")
        new CondensedSnp(tokens(3), tokens(0), tokens(1).toInt, tokens(2).toInt)
    }
  }

  def main(args: Array[String]) {
    val primerDir: File = new File(args(0))
    val files = primerDir.listFiles.filter(_.getName.endsWith(".primer.table.blat.txt"))
    val size: Int = files.size
    println("Found " + size + " primer files")
    var counter = 0

    files.foreach{
      primerFile =>
      val id = primerFile.getName().substring(0, primerFile.getName.indexOf("."))
      counter += 1
      println("Processing file " + counter + " of " + size + ": " + id)
      val out: PrintWriter = new PrintWriter(new FileWriter(new File(primerDir, id + ".primer.table.blat.snps.txt")))
      SnpSearch.processPrimers(primerFile, new File(args(1)), out)
      out.close
    }



  }


}

class SnpInfos(snps: List[CondensedSnp]) {
  println(snps.length)
}

class CondensedSnp(val rsId: String, val chrom: String, val start: Int, val end: Int) {
  val length = end - start
}
