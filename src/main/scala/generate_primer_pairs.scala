#!/bin/sh
exec scala "$0" "$@"
!#

import com.locusdev.primer.{Primer, PrimerTableParser}
import io.Source
import java.io.{PrintStream, File}

if (args.size < 1) {
  println("Usage: generate_primer_pairs input_file output.file")
  println("\tinput_file - a file containing forward and reverse primers in the LocusDev primer table format")
  println("\toutput_file - file where output will be stored")
  System.exit(0)
}

val inputFile = new File(args(0))
require(inputFile.exists, inputFile.getAbsolutePath + " does not exist")

if (inputFile.isFile) {
  require(args.size == 2, "Output file name is required for a single-file input")
  process(inputFile, new File(args(1)))
}
else {
  val files = inputFile.listFiles.filter(_.getName.endsWith(".primer.txt")).toList
  require(!files.isEmpty, inputFile.getAbsolutePath + " does not contain any primer files")
  println("Fount Primer Files: " + files.length)
  files.foreach{
    primerFile =>
    process(primerFile, new File(inputFile, primerFile.getName.substring(0, primerFile.getName.indexOf(".")) + ".primer.pairs.txt"))
  }
}

def process(input: File, output: File) {
  println("Processing: " + input.getAbsolutePath + ", output = " + output.getName)
  val out = new PrintStream(output)

  //parse out all the primers
  val primers = PrimerTableParser.parse(Source.fromFile(input).getLines.filter(_.split("\t").length != 1)).toList.partition(_.direction == "forward")
  val forwardPrimers = primers._1
  val reversePrimers = primers._2

  println("Found forward primers: " + forwardPrimers.size)
  println("Found reverse primers: " + reversePrimers.size)

  //go through all the forward primers and pair them up with all the reverse primers
  val primerPairs = forwardPrimers.map {
    forward =>
      reversePrimers.map(new PrimerPair(forward, _))
  }.reduceLeft(_ ::: _)

  out.println("NUMBER\tFORWARD PRIMER\tREVERSE PRIMER\tLENGTH")

  var counter = 0
  primerPairs.sortBy[Int](_.length).foreach {
    primer =>
      counter += 1
      out.println(counter + "\t" + primer.toString)
  }

  out.close
}

class PrimerPair(val forward: Primer, val reverse: Primer) {
  override def toString = forward.sequence + "\t" + reverse.sequence + "\t" + length

  val length = reverse.endPosition - forward.startPosition
}


