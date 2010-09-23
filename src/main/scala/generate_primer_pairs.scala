#!/bin/sh
exec scala "$0" "$@"
!#

import com.locusdev.primer.{Primer, PrimerTableParser}
import io.Source
import java.io.{PrintStream, PrintWriter, FileWriter, File}

if (args.size < 1) {
  println("Usage: generate_primer_pairs input_file output.file")
  println("\tinput_file - a file containing forward and reverse primers in the LocusDev primer table format")
  println("\toutput_file - file where output will be stored")
  System.exit(0)
}

val input = new File(args(0))
require(input.exists, input.getAbsolutePath + " does not exist")
require(input.isFile, input.getAbsolutePath + " does is not a file")

val out = {
  if(args.size > 1){
    new PrintStream(new File(args(1)))
  }
  else{
    System.out
  }
}

//parse out all the primers
val primers = PrimerTableParser.parse(Source.fromFile(input).getLines).toList.partition(_.direction == "forward")
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
primerPairs.sortBy[Int](_.length).foreach{
  primer =>
  counter += 1
  out.println(counter + "\t" + primer.toString)
}

if(args.size > 1){
  //we were printing to a file and need to close
  out.close
}

class PrimerPair(val forward: Primer, val reverse: Primer) {
  override def toString = forward.sequence + "\t" + reverse.sequence + "\t" + length

  val length = reverse.endPosition - forward.startPosition
}


