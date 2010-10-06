package com.locusdev

import hlatool.DbMhcParser
import java.io.FileReader
import java.lang.String

/**
 * Created by IntelliJ IDEA.
 * User: alexfurman
 * Date: Jun 29, 2010
 * Time: 10:12:35 AM
 * To change this template use File | Settings | File Templates.
 */

object HLABDumper {
  def main(args: Array[String]) {
    if (args.length != 1) {
      println("Usage: HLABDumper referenceAllele")
      System.exit(0);
    }

    val data = DbMhcParser.extractSequence(new FileReader("/Users/alexfurman/projects/locustools/src/main/resources/dbMHC_allelev2.28.xml"),
      "HLA-B", Array("Exon2", "Exon3"), 6)

    //now iterate over the data and dump it out to stdout

    //print reference allele in full
    val referenceBlocks = data(args(0))

    print(args(0));

    referenceBlocks.keys.toList.sorted.foreach {
      key =>
        val referenceSequence = referenceBlocks(key)
        for (i <- 0.until(referenceSequence.length)) {
          print("\t" + referenceSequence.charAt(i))
        }
        //block separator
        print("\t|")
    }
    //new line
    print("\n")


    data.foreach {
      tuple =>
      //no need to reprint the reference
        if (tuple._1 != args(0)) {
          print(tuple._1)

          val blocks = tuple._2
          blocks.keys.toList.sorted.foreach {
            key =>
              var sequence = blocks(key)
              var referenceSequence = referenceBlocks(key)


              for (i <- 0.until(sequence.length)) {
                print("\t")
                if (sequence.charAt(i) == referenceSequence.charAt(i)) {
                  print("_")
                }
                else {
                  print(sequence.charAt(i))
                }
              }
              print("\t|")
          }
          print("\n")
        }
    }

  }
}