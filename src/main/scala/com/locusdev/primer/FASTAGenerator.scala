package com.locusdev.primer

import java.io.{PrintWriter, BufferedWriter, FileWriter, File}
import com.locusdev.util.CSVMapParser

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

    if (args.length != 2) {
      System.exit(-1);
    }

    //check if file exists
    if (!new File(args(0)).exists()) {
      println(args(0) + " does not exist");
      System.exit(-1);
    }

    //check if the specified dir exists and if not try to create it
    val outdir = new File(args(1))
    if (!outdir.exists) {
      if (!outdir.mkdirs) {
        throw new IllegalStateException("Cannot create output dir @ " + args(1))
      }
    }

    //open the file
    val lines = io.Source.fromFile(args(0)).getLines()
    val parsed: List[scala.collection.Map[String, String]] = CSVMapParser.parse(lines, "\t");

    parsed.foreach {
      line =>
        val template = line("Template")
        val primerSet = line("Primer Set")
        if (template.length != 0 && primerSet.length != 0) {
          writeFASTA(outdir, primerSet, template)
        }
    }
  }

  def writeFASTA(outdir: File, primerSet: String, sequence: String) = {
    val writer = new PrintWriter(new FileWriter(new File(outdir, primerSet + ".fasta")))
    writer.println(">" + primerSet);
    writer.println(sequence)
    writer.close

  }

  def printUsage() {
    println("Usage: FASTAGenerator input outputDirectory");
    println("\tThe output directory will be created if it does not already exist");
    println("\tAll the files will be overwritten without warning")
  }
}