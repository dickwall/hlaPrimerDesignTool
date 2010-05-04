package com.locusdev.blast

import java.io.File

/**
 * Takes a Locus Amplicon Sequences spreadsheet and turns it into a bunch of individual FASTA files in a given
 * directory
 * User: alexfurman
 * Date: May 3, 2010
 * Time: 2:45:33 PM
 */

object FASTAGenerator {
  def main(args: Array[String]) {
    printUsage();

    if(args.length != 2){
      System.exit(-1);
    }

    //check if file exists
    if (!new File(args(0)).exists()) {
      println(args(0) + " does not exist");
      System.exit(-1);
    }

    //open the file
    val lines = io.Source.fromFile(args(0)).getLines;
    val parsed:List[scala.collection.Map[String, String]] = CSVParser.parse(lines, "\t");

    println(parsed)

  }

  def printUsage() {
    println("Usage: FASTAGenerator input outputDirectory");
    println("\tThe output directory will be created if it does not already exist");
    println("\tAll the files will be overwritten without warning")
  }
}