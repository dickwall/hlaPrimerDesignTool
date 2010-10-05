#!/bin/sh
exec scala "$0" "$@"
!#


import io.Source
import collection._
import java.io.{FileWriter, PrintWriter, File}

/**
 * Runs p3 for all *.p3.input.txt files in a directory
 *
 * /usr/local/primer3-2.2.2-beta/src/primer3_core -p3_settings_file ./primer3_PrimerList_6-30-2010.txt -output=APOE_01.p3.output.txt APOE_01.p3.input.txt
 */

val P3_BINARY = "/usr/local/primer3-2.2.2-beta/src/primer3_core"

val PRIMER_REGEX_PREFIX = "PRIMER_(LEFT|RIGHT)_[0-9][0-9]*"

if (args.size != 1) {
  println("Usage: p3_for_all_input inputDir")
  System.exit(-1)
}


//validate input dir
val dir = new File(args(0))

require(dir.exists, dir.getAbsolutePath + " does not exist")
require(dir.isDirectory, dir.getAbsolutePath + " is not a directory")

//look for p3 settings file
val settingsFiles = dir.listFiles().filter(_.getName.endsWith(".p3.settings")).toList

require(settingsFiles.size > 0, "Could not find p3 settings file in " + dir.getAbsolutePath)
require(settingsFiles.size == 1, "Found more than one p3 settings files in " + dir.getAbsolutePath + ": " + settingsFiles)

val settingsFile = settingsFiles(0).getAbsolutePath

//figure out what the task is
val primerTaskLines = Source.fromFile(settingsFiles(0)).getLines().toList.filter(_.startsWith("PRIMER_TASK="))
require(primerTaskLines.size == 1, "Expecting a single primer task line in p3 settings, found " + primerTaskLines.size)

//whether or not we're generating a primer list
val primerList = primerTaskLines(0).contains("PICK_PRIMER_LIST")


println("P3 Settings File: " + settingsFile)

//now look for input files
val inputFiles = dir.listFiles().filter(_.getName.endsWith(".p3.input.txt"))

require(inputFiles.size > 0, "No p3 input files found in " + dir.getAbsolutePath)

println("Found p3 input files: " + inputFiles.size)

//now process files one by one

for (x <- 0 until inputFiles.size) {
  println("Processing input file " + (x + 1) + " of " + inputFiles.size)
  val inputFile = inputFiles(x)
  val id = inputFile.getName().substring(0, inputFile.getName.indexOf("."))
  val outputFile = new File(dir, id + ".p3.output.txt")
  val command = P3_BINARY + " -p3_settings_file " + settingsFile + " -output=" + outputFile.getAbsolutePath + " " + inputFile.getAbsolutePath
  println(command)

  val process = Runtime.getRuntime.exec(command)
  process.waitFor();
  if (process.exitValue() != 0) {
    throw new IllegalStateException("Failed to execute last command, exit code = " + process.exitValue())
  }

  //if we're generating a primer list, we need to filter out the fake primers
  if (primerList) {
    filterP3OutputForPrimerList(outputFile)
  }

}

/**
 * When p3 is asked to generate a primer list (as opposed to primer pairs) its output is hacky. They will still return
 * primer pairs for each primer, however only one of the primers in the pair is real - the other one is filler and needs
 * to be filtered out.
 *
 * This is done the following way: Figure out whether or not the primer "pair" is to the left or to the right of our
 * product region. If it's to the right of the product region, the right primer is real and the left primer is fake and
 * needs to go. If it's to the left of the region, the left primer is real and the right one is fake.
 */
def filterP3OutputForPrimerList(outputFile: File) = {
  val linesList = Source.fromFile(outputFile).getLines.toList
  val lines = linesList.toIterator

  //first line is the primer name
  val primerSequenceIdLine = lines.next
  //the next line is the sequence template
  val sequenceTemplateLine = lines.next
  //the third line is the sequence target. this is the line we care about
  val targetLine = lines.next
  require(targetLine.startsWith("SEQUENCE_TARGET="), "Expecting SEQUENCE_TARGET line, got: " + targetLine)

  val startAndLength = targetLine.split("=")(1).split(",")
  val start = startAndLength(0).toInt
  val end = start + startAndLength(1).toInt

  //save the rest of the header lines and do some sanity checking
  val leftNumLine = lines.next
  require(leftNumLine.startsWith("PRIMER_LEFT_NUM_RETURNED="), "Expecting PRIMER_LEFT_NUM_RETURNED line, got: " + leftNumLine)
  val rightNumLine = lines.next
  require(rightNumLine.startsWith("PRIMER_RIGHT_NUM_RETURNED="), "Expecting PRIMER_RIGHT_NUM_RETURNED line, got: " + rightNumLine)
  val internalNumLine = lines.next
  require(internalNumLine.startsWith("PRIMER_INTERNAL_NUM_RETURNED="), "Expecting PRIMER_INTERNAL_NUM_RETURNED line, got: " + internalNumLine)
  val pairNumLine = lines.next
  require(pairNumLine.startsWith("PRIMER_PAIR_NUM_RETURNED="), "Expecting PRIMER_PAIR_NUM_RETURNED line, got: " + pairNumLine)


  //now throw all the primer positions into a map
  val positions = lines.filter(_.matches("PRIMER_(LEFT|RIGHT)_[0-9][0-9]*=[0-9]*,[0-9]*")).toList


  val primerQuality = new mutable.HashMap[String, Boolean]

  val LEFT = "LEFT"
  val RIGHT = "RIGHT"


  positions.foreach {
    line =>
      val primerStartAndLength = line.split("=")(1).split(",")
      val primerStart = primerStartAndLength(0).toInt
      val primerEnd = primerStart + primerStartAndLength(1).toInt



      val primer = extractPrimerNumber(line)
      if (line.contains(LEFT)) {
        //check to see if the left primer is to the left of the product
        val id = primer + "_LEFT"

        if (primerEnd < start) {
          //this is a left primer
          primerQuality += id -> true
        }
        else {
          primerQuality += id -> false
        }
      }
      else {
        //check to see if the right primer starts after the end of the product
        val id = primer + "_RIGHT"

        if (primerStart > end) {
          //this is a right primer
          primerQuality += id -> true
        }
        else {
          primerQuality += id -> false
        }
      }
  }



  println(primerQuality.values.toList.filter(_ == true).length + " out of " + primerQuality.values.toList.size + " primers are good")
  println(linesList.size + " lines")


  //now that we know what to filter out, we can take care of the writing to a file bit
  val out = new PrintWriter(new FileWriter(outputFile))

  linesList.foreach {
    line =>
      if (!line.matches(PRIMER_REGEX_PREFIX + ".*")) {
        //if this is not a line describing a primer, copy it over
        out.println(line)
      }
      else {
        //this is a line describing an actual primer, filter it by qulity
        val primer = extractPrimerNumber(line)
        val id = primer + (if (line.contains("LEFT")) "_LEFT" else "_RIGHT")
        if (primerQuality(id)) {
          out.println(line)
        }
      }

  }

  out.close


}

private def extractPrimerNumber(line: String) = line.split("_|=")(2)