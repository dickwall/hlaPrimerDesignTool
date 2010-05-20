package com.locusdev.blast

import collection.mutable.ListBuffer
import java.lang.String
import java.io.{FileWriter, PrintWriter, FilenameFilter, File}

/**
 * Given a directory with any number of sequences in the FASTA format, will execute blastn to compare each sequence to
 * the other, saving the output in the "output" subdirectory.
 *
 * Note that for N sequences, this will lead to N^2 executions of balstn and the creation of N^2 files. Depending on sequence
 * length and N, this may take a long time
 *
 * User: alexfurman
 * Date: May 4, 2010
 * Time: 12:30:24 PM
 */

object Blaster {
  def main(args: Array[String]) {
    printUsage();

    if (args.length != 1) {
      System.exit(-1);
    }

    //get all the .fasta files from the passed in directory
    val inputDir = new File(args(0))
    val sequences = inputDir.list.filter {
      name => name.endsWith(".fasta")
    };

    //create the output dir if needed
    val outputDir = new File(inputDir, "blastresults");

    if (!outputDir.exists) {
      if (!outputDir.mkdirs) {
        throw new IllegalStateException("Could not create output dir @ " + outputDir)
      }
    }

    println("Comparing " + sequences.length + " sequences");

    val resultsSummary = new ListBuffer[String]
    val resultsDetail = new ListBuffer[String]

    val summaryWriter = new PrintWriter(new FileWriter(new File(outputDir, "resultsSummary.txt")))
    val detailWriter = new PrintWriter(new FileWriter(new File(outputDir, "resultsDetails.txt")))

    for (x <- 0 until sequences.length) {
      for (y <- (x + 1) until sequences.length) {
        val primer1: String = sequences(x).substring(0, sequences(x).indexOf('.'))
        val primer2: String = sequences(y).substring(0, sequences(y).indexOf('.'))
        val outName = primer1 + "-" + primer2 + ".out"
        val command = "/usr/local/ncbi/blast/bin/blastn " +
                " -query " + inputDir.getAbsolutePath + File.separator + sequences(x) +
                " -subject " + inputDir.getAbsolutePath + File.separator + sequences(y) + " -outfmt 10 " +
                "-out " + outputDir + File.separator + outName + " -perc_identity 90";
        println(command)
        val process = Runtime.getRuntime.exec(command)
        process.waitFor();
        if (process.exitValue() != 0) {
          throw new IllegalStateException("Failed to execute last command, exit code = " + process.exitValue())
        }

        //now get the number of rows from the output file, that is out hit score
        val lines = io.Source.fromPath(outputDir + File.separator + outName).getLines("\n")
        val summary = primer1 + "\t" + primer2 + "\t" + lines.length
        summaryWriter.println(summary)
        summaryWriter.flush
        println("SUMMARY: " + summary)
        lines.foreach { line =>
          val lineString: Any = primer1 + "," + primer2 + "," + line
          detailWriter.print(lineString)
          detailWriter.flush
          print("\tLINE: " + lineString)
        }
      }
    }

    //write out results files
    summaryWriter.close
    detailWriter.close

    resultsSummary.foreach {
      println(_)
    }


  }

  def printUsage() {
    println("Usage: Blaster targetDir")
    println("\tOutput files will be saved in /blastresults subdirectory of the target dir")
  }
}