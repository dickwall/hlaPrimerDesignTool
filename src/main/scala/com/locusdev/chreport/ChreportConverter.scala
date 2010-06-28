package com.locusdev.chreport

import xml.pull.XMLEventReader
import java.io.File
import io.Source
import java.lang.String
import collection.Iterator

/**
 * Converts the ginormous ncbi Chromosome Report files into tab-delimited files containing only the information we need
 *
 * User: alexfurman
 * Date: Jun 15, 2010
 * Time: 12:59:51 PM
 */

object ChreportConverter {
  def convertXml(inputFile: File, writer: java.io.Writer) = {
    val start = System.currentTimeMillis;
    var events = 0;
    println("parsing chromosome xml")

    val source: Source = io.Source.fromFile(inputFile)
    val lines: Iterator[String] = source.getLines()

    while(lines.hasNext){
      val line = lines.next
      events += 1

      //if(events % 100000 == 0){
        println(events + ": " + line)
      //}
    }

//    val p = new XMLEventReader(io.Source.fromFile(inputFile))
//    var bEnd: Boolean = false
//    while (p.hasNext) {
//      var next = p.next
//
//
//      events += 1
//
//      if(events % 10000 == 0){
//        println(events + ": " + next)7
//      }
//    }

    println("parsed chromosome xml in " + ((System.currentTimeMillis - start) / 1000) + " seconds")
  }


  def main(args: Array[String]) {
    ChreportConverter.convertXml(new File("/Users/alexfurman/projects/chr_reports/ds_flat_ch1.flat"), null)
  }
}