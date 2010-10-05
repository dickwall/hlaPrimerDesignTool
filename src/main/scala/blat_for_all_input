#!/bin/sh
exec scala "$0" "$@"
!#


import java.io.File

/**
 * Runs blat for all *.blat.input.fa files in a directory
 *
 * /usr/local/primer3-2.2.2-beta/src/primer3_core -p3_settings_file ./primer3_PrimerList_6-30-2010.txt -output=APOE_01.p3.output.txt APOE_01.p3.input.txt
 */


if (args.size != 2) {
  println("Usage: blat_for_all_input inputDir blatBinary")
  System.exit(-1)
}


//validate input dir
val dir = new File(args(0))

require(dir.exists, dir.getAbsolutePath + " does not exist")
require(dir.isDirectory, dir.getAbsolutePath + " is not a directory")

//now look for input files
val inputFiles = dir.listFiles().filter(_.getName.endsWith(".blat.input.fa"))

require(inputFiles.size > 0, "No blat input files found in " + dir.getAbsolutePath)

println("Found blat input files: " + inputFiles.size)

//now process files one by one

for (x <- 0 until inputFiles.size) {
  println("Processing input file " + (x + 1) + " of " + inputFiles.size)
  val inputFile = inputFiles(x)
  val id = inputFile.getName().substring(0, inputFile.getName.indexOf("."))
  val outputFile = new File(dir, id + ".blat.output.txt")
  val command = args(1) + " -stepSize=5 -repMatch=2253 -minScore=20 -oneOff=1 -minIdentity=0 ../golden_path/chromosomelist.txt " + inputFile.getAbsolutePath + " " + outputFile.getAbsolutePath
  println(command)

  val process = Runtime.getRuntime.exec(command)
  process.waitFor();
  if (process.exitValue() != 0) {
    throw new IllegalStateException("Failed to execute last command, exit code = " + process.exitValue())
  }
}