#!/bin/sh
exec scala "$0" "$@"
!#
import collection.immutable.List
import collection.mutable.ArrayBuffer
import io.Source
import java.io.{FileWriter, PrintWriter, File}

if (args.length < 1) {
  println("Usage: add_blat_to_table inputDir")
  println("\tinput - directory containing blat output files and primer files in the locus tabular format")
  println("\tblat output files need to end in *.blat.output.txt and have corresponding *.primer.table.txt in the same directory")
  System.exit(-1)
}

val inputDir = new File(args(0))
require(inputDir.exists, inputDir.getAbsolutePath + " does not exist")
require(inputDir.isDirectory, inputDir.getAbsolutePath + " is not a directory")

val blatOutputFiles = inputDir.listFiles.filter(_.getName.endsWith(".blat.output.txt"))
require(blatOutputFiles.size > 0, inputDir + " does not contain any blat output files matching *.blat.output.txt")

//now, process each file
blatOutputFiles.foreach {
  blatFile =>
  //parse out the name and find the appropriate primer table file
    val id = blatFile.getName().substring(0, blatFile.getName.indexOf("."))
    val primerTableFile = new File(blatFile.getParent, id + ".primer.table.txt")
    require(primerTableFile.exists, "No corresponding primer table file for " + id + ", expecting: " + primerTableFile.getAbsolutePath)

    //file we're going to be outputing to
    val outputFile = new File(blatFile.getParent, id + ".primer.table.blat.txt")

    processBlatOutput(blatFile, primerTableFile, outputFile)
}

def processBlatOutput(blatFile: File, primerTableFile: File, outputFile: File) = {
  println(blatFile)
  println(primerTableFile)

  //get a list of hits per primer
  val blatHits = parseBlatOuptut(Source.fromFile(blatFile).getLines())


  //now, go through the table line-by-line and add hits information towards the end of each line
  val tableLines = Source.fromFile(primerTableFile).getLines

  //transfer the header line over
  val out = new PrintWriter(new FileWriter(outputFile))

  //transfer the header
  out.print(tableLines.next)
  out.println("\tPERFECT MATCHES\tNEAR MATCHES")

  //go line-by-line
  var counter = 0
  println("processing primer table")
  tableLines.filter(_.trim.length > 0).foreach {
    line =>
      counter += 1
      if (counter % 500 == 0) {
        println("\tlines processed: " + counter)
      }

      val primer = line.split("\t")(0)

      //transfer the line
      out.print(line)

      //number of perfect matches
      val blatHitOption = blatHits.get(primer)

      val perfectMatches = blatHitOption match {
        case Some(blatHit) => {
          val primary = if (qscore(blatHit.primaryHit) == 1) 1 else 0
          primary + blatHit.secondaryHits.filter(qscore(_) == 1).size
        }
        case None => 0
      }

      val nearMatches = blatHitOption match {
        case Some(blatHit) => {
          val primary = if (qscore(blatHit.primaryHit) < 1) 1 else 0
          primary + blatHit.secondaryHits.filter {
            hit =>
              val score = qscore(hit)
              score < 1 && score >= .874
          }.size
        }
        case None => 0
      }

      out.println("\t" + perfectMatches + "\t" + nearMatches)
  }
  println("done processing primer table")
  out.close

}

def parseBlatOuptut(lines: Iterator[String]) = {
  println("Parsing blat output")
  val resultsMap = new collection.mutable.HashMap[String, BlatHits]

  //skip header
  (lines.next)
  (lines.next)
  (lines.next)
  (lines.next)
  (lines.next)


  println("about to group by primer")
  val mapped = new collection.mutable.HashMap[String, ArrayBuffer[Array[String]]]

  var counter = 0
  lines.foreach {
    line =>
      counter += 1
      if (counter % 100000 == 0) {
        println("\tlines processed: " + counter)
      }
      val tokens = line.split("\t")
      if (!mapped.contains(tokens(9))) {
        mapped += tokens(9) -> new collection.mutable.ArrayBuffer[Array[String]]
      }
      mapped(tokens(9)) += (tokens)
  }

  println("counverting into objects")

  counter = 0

  val hits = mapped.map {
    case (primer, hitsList) => {
      counter += 1
      if (counter % 1000 == 0) {
        println("\tprimers processed: " + counter)
      }
      val sortedHits = hitsList.sortBy(qscore(_)).toList.reverse
      (primer, new BlatHits(primer, sortedHits.head, sortedHits.tail))
    }
  }

  println("done converting into objects")
  hits
}


class BlatHits(val primer: String, val primaryHit: Array[String], val secondaryHits: List[Array[String]]) {

  //require(qscore(primaryHit) == 1, "Primer " + primer + " does not have a 100% hit")
  override def toString = primer + "[secondary hits=" + secondaryHits.length + "]"
}

def qscore(blat: Array[String]) = {
  //QScore= (matches - mismatch -Tgapbases - Qgapbases) / Q-Size
  (blat(0).toDouble - blat(1).toDouble - blat(5).toDouble - blat(7).toDouble) / blat(10).toDouble
}