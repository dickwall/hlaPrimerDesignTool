#!/bin/sh
exec scala "$0" "$@"
!#
import collection.immutable.List
import io.Source
import java.io.{PrintWriter, FileWriter, File}

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
  tableLines.filter(_.trim.length > 0).foreach {
    line =>
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
  out.close

}

def parseBlatOuptut(lines: Iterator[String]) = {
  val resultsMap = new collection.mutable.HashMap[String, BlatHits]

  val linesList = lines.toList

  val truncated = linesList.slice(5, linesList.length - 1)

  for (x <- 0 until truncated.length) {
    if (truncated(x).split("\t").length < 10) {
      println(x + ": " + truncated(x))
    }
  }


  val mapped = truncated.map(_.split("\t")).groupBy(_(9))

  val hits = mapped.map {
    case (primer, hitsList) => {
      val sortedHits = hitsList.sortBy(qscore(_)).toList.reverse
      (primer, new BlatHits(primer, sortedHits.head, sortedHits.tail))
    }
  }

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