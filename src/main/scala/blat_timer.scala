#!/bin/sh
exec scala "$0" "$@"
!#

import java.io.{PrintWriter, File}

/**
 * Timer script that will execute blat for increasing numbers of primers and report how long it takes.
 */


//blat will be executed once for each N where N is the number of primers we're blating
val runs = List(1, 10, 50, 100, 500, 1000, 2500, 5000, 10000, 15000, 20000)

if (args.length < 2) {
  println("Usage blat_timer primer_file blat_binary")
  println("\tWhere primer_file is a fasta file with one primer per 2 lines that contains at least as many primers as the biggest N")
  println("\tblat_binary points to the blat executable")
  System.exit(0)
}

val file: File = new File(args(0))
require(file.exists() && file.isFile, "File does not exist or is a directory: " + file.getAbsolutePath)

//now, kick off blast
runs.foreach {
  runLength =>
  //create the appropriate input file if we need to
    val newFile = new File(file.getParent, "blat_timer_" + runLength + "_primers.fa")
    if (!newFile.exists) {
      copyNLines(file, newFile, runLength * 2)
    }

    //kick off blat and time it
    val start = System.currentTimeMillis
    println("Kicking off blat for N = " + runLength)
    val command = args(1) + " -stepSize=5 -repMatch=2253 -minScore=20 -oneOff=1 -minIdentity=0 ../golden_path/chromosomelist.txt " + newFile.getAbsolutePath + " " + newFile.getAbsolutePath + ".out"
    println(command)
    val process = Runtime.getRuntime.exec(command)
    process.waitFor();
    println("Blat for N = " + runLength + "took: " + ((System.currentTimeMillis - start)/1000) + " seconds")

}

def copyNLines(from: java.io.File, to: File, n: Int) {
  val out = new PrintWriter(new java.io.BufferedWriter(new java.io.FileWriter(to)));
  val lines = io.Source.fromFile(from).getLines.toList.slice(0, n)

  for (line <- lines) {
    out.println(line)
  }
  out.close()
}