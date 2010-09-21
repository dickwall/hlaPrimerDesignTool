package com.locusdev.primer

import com.locusdev.model.Exon
import collection._
import io.Source
import java.io.{PrintWriter, FileWriter, File}

/**
 * Created by IntelliJ IDEA.
 * User: afurman
 * Date: Aug 23, 2010
 * Time: 3:32:19 PM
 * To change this template use File | Settings | File Templates.
 */

class P3Parser(lines: Iterator[String]) {
  //the first line is always PRIMER_SEQUENCE_ID
  val primerSequenceId = lines.next.split("=")(1)

  //parse out the things we care about in the primer sequence id
  //Example: APOE_01[19:45389028,45429108]
  private val primerSequenceIdTokens = primerSequenceId.split("[<>,:]")
  private val chromosome = primerSequenceIdTokens(1)
  private val blockStart = primerSequenceIdTokens(2).toInt
  private val blockEnd = primerSequenceIdTokens(3).toInt


  private val primerMap = new mutable.HashMap[String, Map[String, String]]

  //match all lines describing actual primers
  lines.filter(_.matches(P3Parser.PRIMER_REGEX_PREFIX + ".*")).foreach {


    line =>
    //all the lines that we care about (the ones that actually describe primers) are int he following
    //format PRIMER_*_NUMBER_** where * can either be (RIGHT, LEFT, or PAIR) and ** is the actual
    //property. Any lines that don't follow this format can be ignored.
      val primer = P3Parser.extractPrimerNumber(line)

      if (!primerMap.contains(primer)) {
        //prime the map if this is the first entry for our primer
        primerMap.put(primer, new mutable.HashMap[String, String])
      }

      //left or right
      val side = line.split("_")(1)

      //pull out the primer position
      if (line.matches(P3Parser.PRIMER_REGEX_PREFIX + "=[0-9]*,[0-9]*")) {
        primerMap(primer) += (side + "_POSITION") -> line.split("=")(1)
      }
      else {
        //everything besides position looks the same - everything after the primer number is the
        //key, everything after the "=" is the value
        val pair = line.substring(line.indexOf("_" + primer) + 2 + primer.size).split("=")
        primerMap(primer) += side + "_" + pair(0) -> pair(1)
      }
  }

  def formatTabularOutput() = {
    val out = new StringBuffer()
    out.append(outputLine("PRIMER_SEQUENCE", "PRIMER_START_POSITION", "PRIMER_PENALTY", "PRIMER_TM",
      "PRIMER_GC_PERCENT", "PRIMER_SELF_ANY_TH", "PRIMER_SELF_END_TH", "PRIMER_HAIRPIN_TH", "PRIMER_END_STABILITY", "EXON", "CHROMOSOME",
      "PRIMER_START_POS", "PRIMER_END_POS", "PRIMER_DIRECTION"))

    /*
    * The primer map is set up by primer number, and each entry contains data for a left primer and a right primer
    * For the purposes of this tabular output, the "left" and "right" part is irrelevant. Moreover, if input
    * was generated as a primer list by p3, it is also often wrong. Each entry, therefore, is treated as two
    * individual and unrelated primers
    */
    primerMap.keys.toList.sortBy(_.toInt).foreach {
      primer =>
        val data = primerMap(primer)
        if (data.contains("LEFT_SEQUENCE")) {
          out.append(extractTableLine("LEFT", data))
        }
        //line may or may not contain a "right" entry if we're a primer list
        if (data.contains("RIGHT_SEQUENCE")) {
          out.append(extractTableLine("RIGHT", data))
        }

    }
    out.toString
  }

  def formatBlatInput() = {
    val out = new StringBuffer

    primerMap.keys.toList.sortBy(_.toInt).foreach {
      primer =>
        val data = primerMap(primer)

        if (data.contains("LEFT_SEQUENCE")) {
          val left_sequence = data("LEFT_SEQUENCE")
          out.append(">" + left_sequence + "\n")
          out.append(left_sequence + "\n")
        }

        if (data.contains("RIGHT_SEQUENCE")) {
          val right_sequence = data("RIGHT_SEQUENCE")
          out.append(">" + right_sequence + "\n")
          out.append(right_sequence + "\n")
        }
    }

    out.toString
  }

  private def extractTableLine(side: String, data: Map[String, String]) = {
    val out = new StringBuffer()
    val startPosition: Int = data(side + "_POSITION").split(",")(0).toInt
    val length: Int = data(side + "_POSITION").split(",")(1).toInt

    /**
     * Left primers start at the beginning and go right while right primers start at the end and go left. We need to
     * keep this in mind when converting to start and end coordinates
     */
    val primerStart = if (side == "LEFT") startPosition + blockStart else startPosition - length + blockStart + 1
    val primerEnd = if (side == "LEFT") (startPosition + blockStart + length) else startPosition + blockStart + 1


    out.append(outputLine(data(side + "_SEQUENCE"), startPosition.toString, data(side + "_PENALTY"),
      data(side + "_TM"), data(side + "_GC_PERCENT"), data(side + "_SELF_ANY_TH"), data(side + "_SELF_END_TH"),
      data(side + "_HAIRPIN_TH"), data(side + "_END_STABILITY"), primerSequenceId, chromosome, primerStart.toString,
      primerEnd.toString, if (side == "LEFT") "forward" else "reverse"))
    out.toString
  }

  /**
   *  PRIMER_LEFT_9_PENALTY=0.039330
   * PRIMER_RIGHT_9_PENALTY=0.039330
   * PRIMER_LEFT_9_SEQUENCE=GTGAACCCAGTAGGAGCCATCTTCA
   * PRIMER_RIGHT_9_SEQUENCE=GAAGATGGCTCCTACTGGGTTCACA
   * PRIMER_LEFT_9=1346,25
   * PRIMER_RIGHT_9=1369,25
   * PRIMER_LEFT_9_TM=65.961
   * PRIMER_RIGHT_9_TM=65.961
   * PRIMER_LEFT_9_GC_PERCENT=52.000
   * PRIMER_RIGHT_9_GC_PERCENT=52.000
   * PRIMER_LEFT_9_SELF_ANY_TH=0.00
   * PRIMER_RIGHT_9_SELF_ANY_TH=0.00
   * PRIMER_LEFT_9_SELF_END_TH=0.00
   * PRIMER_RIGHT_9_SELF_END_TH=0.00
   * PRIMER_LEFT_9_HAIRPIN_TH=37.40
   * PRIMER_RIGHT_9_HAIRPIN_TH=34.36
   * PRIMER_LEFT_9_END_STABILITY=3.0200
   * PRIMER_RIGHT_9_END_STABILITY=3.5800
   */


  def formatPgrepInput() = {
    val out = new StringBuilder()
    out.append(outputLine("primer name", "forward sequence", "reverse sequence", "product size"))
    //go through primer by primer
    primerMap.keys.toList.sortBy(_.toInt).foreach {
      primer =>

        val data = primerMap(primer)
        val productSize = getProductSize(data)

        out.append(outputLine(primer + "_", data("RIGHT_SEQUENCE"), data("LEFT_SEQUENCE"), productSize))
    }
    out.toString
  }

  /**
   * Calculate product size from left and right primer locations when it's not supplied
   */
  private def getProductSize(primerEntry: Map[String, String]) = {
    if (primerEntry.contains("PRODUCT_SIZE")) {
      primerEntry("PRODUCT_SIZE")
    }
    else {
      throw new IllegalStateException("No product size in p3 output. Did you generate a primer " +
              "list instead of primer pairs?")
    }
  }

  /**
   * output a line in the pgrep input format
   * note that we must append a "_" to the primer name in order to avoid a bug in pgrep
   */
  private def outputLine(names: String*) = {
    val out = new StringBuilder();
    for (x <- 0.until(names.size)) {
      out.append(names(x))
      if (x != names.size - 1) out.append("\t")
    }
    out.append("\n")
    out.toString
  }
}

object P3Parser {
  val PRIMER_REGEX_PREFIX = "PRIMER_(LEFT|RIGHT)_[0-9][0-9]*"

  /**
   * Takes a list of exons and returns a list of p3 input strings
   */
  def createP3Input(chromosomeDir: File, exons: List[Exon], margin: Int, size: Int): Map[Exon, String] = {
    //optimizaiton method - partition the exons by chromosome and handle them together to not have to reparse chromosome
    //sequence every time, which is resourse - heavy

    val resultsMap = new collection.mutable.HashMap[Exon, String]()

    val byChromosome = exons.groupBy(_.chromosome)
    byChromosome.keys.foreach {
      chromosome =>
        println("parsing out chromosome " + chromosome)
        val chromSequence = FASTAExtractor.getFullChromosomeSequence(chromosomeDir, chromosome);
        byChromosome(chromosome).foreach {
          exon: Exon =>
            println("\t" + exon)
            resultsMap += exon -> formatP3Input(chromSequence, exon, margin, size)
        }
    }
    Map[Exon, String]() ++ resultsMap
  }

  def createP3Input(chromosomeDir: File, exon: Exon, margin: Int, size: Int): String = {
    createP3Input(chromosomeDir, exon :: List[Exon](), margin, size)(exon)
  }

  private def extractPrimerNumber(line: String) = line.split("_|=")(2)


  def formatP3Input(chromosomeSequence: String, exon: Exon, margin: Int, size: Int): String = {
    val targetLength: Int = exon.start - exon.end
    val start = exon.start - margin - size;
    val end = exon.end + margin + size;

    val sequence = FASTAExtractor.extractSequence(chromosomeSequence, start, end)

    val sb = new StringBuilder("PRIMER_SEQUENCE_ID=" + exon.name + "<" + exon.chromosome + ":" + start + "," + end + ">" + "\n")
    sb.append("SEQUENCE_TEMPLATE=" + sequence + "\n")
    sb.append("SEQUENCE_TARGET=" + size + "," + (exon.end - exon.start + margin * 2) + "\n")
    sb.append("=")

    sb.toString
  }

  /**
   * When p3 is asked to generate a primer list (as opposed to primer pairs) its output is hacky. They will still return
   * primer pairs for each primer, however only one of the primers in the pair is real - the other one is filler and needs
   * to be filtered out.
   *
   * This is done the following way: Figure out whether or not the primer "pair" is to the left or to the right of our
   * product region. If it's to the right of the product region, the right primer is real and the left primer is fake and
   * needs to go. If it's to the left of the region, the left primer is real and the right one is fake.
   */
  def filterP3OutputForPrimerList(outputFile: File) = {
    val linesList = Source.fromFile(outputFile).getLines.toList
    val lines = linesList.toIterator

    //first line is the primer name
    val primerSequenceIdLine = lines.next
    //the next line is the sequence template
    val sequenceTemplateLine = lines.next
    //the third line is the sequence target. this is the line we care about
    val targetLine = lines.next
    require(targetLine.startsWith("SEQUENCE_TARGET="), "Expecting SEQUENCE_TARGET line, got: " + targetLine)

    val startAndLength = targetLine.split("=")(1).split(",")
    val start = startAndLength(0).toInt
    val end = start + startAndLength(1).toInt

    //save the rest of the header lines and do some sanity checking
    val leftNumLine = lines.next
    require(leftNumLine.startsWith("PRIMER_LEFT_NUM_RETURNED="), "Expecting PRIMER_LEFT_NUM_RETURNED line, got: " + leftNumLine)
    val rightNumLine = lines.next
    require(rightNumLine.startsWith("PRIMER_RIGHT_NUM_RETURNED="), "Expecting PRIMER_RIGHT_NUM_RETURNED line, got: " + rightNumLine)
    val internalNumLine = lines.next
    require(internalNumLine.startsWith("PRIMER_INTERNAL_NUM_RETURNED="), "Expecting PRIMER_INTERNAL_NUM_RETURNED line, got: " + internalNumLine)
    val pairNumLine = lines.next
    require(pairNumLine.startsWith("PRIMER_PAIR_NUM_RETURNED="), "Expecting PRIMER_PAIR_NUM_RETURNED line, got: " + pairNumLine)


    //now throw all the primer positions into a map
    val positions = lines.filter(_.matches("PRIMER_(LEFT|RIGHT)_[0-9][0-9]*=[0-9]*,[0-9]*")).toList


    val primerQuality = new mutable.HashMap[String, Boolean]

    val LEFT = "LEFT"
    val RIGHT = "RIGHT"


    positions.foreach {
      line =>
        val primerStartAndLength = line.split("=")(1).split(",")
        val primerStart = primerStartAndLength(0).toInt
        val primerEnd = primerStart + primerStartAndLength(1).toInt



        val primer = extractPrimerNumber(line)
        if (line.contains(LEFT)) {
          //check to see if the left primer is to the left of the product
          val id = primer + "_LEFT"

          if (primerEnd < start) {
            //this is a left primer
            primerQuality += id -> true
          }
          else {
            primerQuality += id -> false
          }
        }
        else {
          //check to see if the right primer starts after the end of the product
          val id = primer + "_RIGHT"

          if (primerStart > end) {
            //this is a right primer
            primerQuality += id -> true
          }
          else {
            primerQuality += id -> false
          }
        }
    }

    primerQuality.keys.toList.sortWith(_.split("_")(0).toInt < _.split("_")(0).toInt).foreach {
      key =>
        println(key + " = " + primerQuality(key))
    }

    println(primerQuality.values.toList.filter(_ == true).length + " out of " + primerQuality.values.toList.size + " primers are good")
    println(linesList.size + " lines")


    //now that we know what to filter out, we can take care of the writing to a file bit
    val out = new PrintWriter(new FileWriter(outputFile))

    linesList.foreach {
      line =>
        if (!line.matches(PRIMER_REGEX_PREFIX + ".*")) {
          //if this is not a line describing a primer, copy it over
          out.println(line)
        }
        else {
          //this is a line describing an actual primer, filter it by qulity
          val primer = extractPrimerNumber(line)
          val id = primer + (if (line.contains("LEFT")) "_LEFT" else "_RIGHT")
          if (primerQuality(id)) {
            out.println(line)
          }
        }

    }

    out.close


  }
  //driver method, should be erased
  def main(args: Array[String]) {
    P3Parser.filterP3OutputForPrimerList(new File(args(0)))
  }
}