#!/bin/sh
exec scala "$0" "$@"
!#

/**
 * This is a script that takes primer3 output (a flat Boulder-IO pair-list) from standard in and converts
 * it to a pgrep input file (tab delimited) into standard out.
 *
 * I am purposefully not going with a full Boulder-IO implementation here - the output that primer3 produces
 * is simple enough that there's not need for the overhead
 */
import com.locusdev.primer.P3Parser
import io.Source
import java.io.{PrintWriter, FileWriter, File}
import java.lang.String
import scala.collection._


if (args.size < 2) {
  printUsage
  System.exit(-37)
}

//require a format
val format = args(0)
require(format == "table" || format == "blat", "Illegal format type: " + format)
//require a valid input
val inputFile = new File(args(1))
require(inputFile.exists, "Input file/directory does not exist: " + inputFile.getAbsolutePath)
//if input is a file, require an output file
if (inputFile.isFile) {
  require(args.length >= 3, "Input is a file, you must provide output file")
}

//create a map of input file to output file

val files: List[(File, File)] = if (inputFile.isFile) {
  (inputFile -> new File(args(2))) :: List()
}
else {
  val postfix = if (format == "table") ".primer.table.txt" else ".blat.input.fa"
  //look for files that end with *.p3.output.txt
  inputFile.listFiles.filter(_.getName.endsWith(".p3.output.txt")).map {
    file =>
      (file, new File(inputFile, file.getName.substring(0, file.getName.indexOf(".")) + postfix))
  }.toList
}

println("Processing " + files.size + " files")


files.foreach {
  case (input, output) => {
    println("\t-" + input.getName)
    val parser: P3Parser = new P3Parser(Source.fromFile(input).getLines)
    val results = if (format == "table") parser.formatTabularOutput() else parser.formatBlatInput()

    //print the results to file
    val writer = new PrintWriter(new FileWriter(output))
    println("\t\twriting " + output.getAbsolutePath)
    writer.println(results)
    writer.close
  }
}



def printUsage: Unit = {
  println("Usage: p3_converter format_type input output")
  println("-fromat_type can be blat|table")
  println("-input can be a single file or a directory containing *.p3.output.txt files")
  println("-if input is a single file, then output file is required. otherwise, output will be placed in *.blat.input.fa files in the same directory")
}






