package com.locusdev.primer

import io.Source
import com.locusdev.util.CSVParser
import com.locusdev.model.Exon
import scala.collection._

/**
 * Parses an exon list in the following format (tab-delimited). The first 4 columns are required, the rest is optional
 *
 *  "Gene_Amplicon" "Chrom" "exonStarts"    "exonEnds"      "Length"        "Distance to Next Exon" "Notes"
 *  "DPYD_01"       1       97543301        97544702        1401    3183
 *  "DPYD_02"       1       97547885        97548026        141     16018
 *  "DPYD_03"       1       97564044        97564188        144     94436
 *  "DPYD_04"       1       97658624        97658804        180     41603
 *  "DPYD_05"       1       97700407        97700550        143     70264
 *  "DPYD_06"       1       97770814        97770934        120     798 
 *
 *
 * User: afurman
 * Date: Aug 20, 2010
 * Time: 11:07:27 AM
 */

object ExonListParser {
  def parseExonList(fileLocation: String): List[Exon] = {
    val lines: Iterator[String] = Source.fromFile(fileLocation).getLines()
    parseExonList(lines)
  }

  def parseExonList(lines: Iterator[String]): List[Exon] = {
    //we allow for optional columns
    val parsedLines = CSVParser.parse(lines, "\t", false)
    val parsed = parsedLines.map {
      line =>
        requireColumn(line, "Gene_Amplicon")
        requireColumn(line, "Chrom")
        requireColumn(line, "exonStarts")
        requireColumn(line, "exonEnds")

        val distance = line.get("Distance to Next Exon") match {
          case None => None
          case Some(x) => Some(x.toInt)

        }

        new Exon(line("Gene_Amplicon"), formatChrom(line("Chrom")), line("exonStarts").toInt, line("exonEnds").toInt,
          distance, line.get("Notes"))
    }

    parsed
  }

  def formatChrom(chrom: String) = {
    if (chrom.startsWith("chr")) chrom.substring(3) else chrom
  }

  def requireColumn(line: Map[String, String], column: String) = {
    require(line.contains(column), "missing requred column: \"" + column + "\"")
  }
}