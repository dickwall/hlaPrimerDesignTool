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
import java.lang.String
import scala.io._
import scala.collection._

val outputType = if (args.size > 0) args(0) else "";

if (outputType != "pgrep" && outputType != "table") {
  println("USAGE: p3_converter output_type")
  println("-output_type can be \"pgrep\" or \"table\" for pgrep or Locus tabular format")
  System.exit(-1)
}

//get lines from stdin
val lines = Source.fromInputStream(System.in).getLines();

val REGEX_PREFIX = "PRIMER_(LEFT|RIGHT)_[0-9][0-9]*"



val primerMap = new mutable.HashMap[String, Map[String, String]]

//match all lines describing actual primers
lines.filter(_.matches(REGEX_PREFIX + ".*")).foreach {


  line =>
  //all the lines that we care about (the ones that actually describe primers) are int he following
  //format PRIMER_*_NUMBER_** where * can either be (RIGHT, LEFT, or PAIR) and ** is the actual
  //property. Any lines that don't follow this format can be ignored.
    val primer = extractPrimerNumber(line)

    if (!primerMap.contains(primer)) {
      //prime the map if this is the first entry for our primer
      primerMap.put(primer, new mutable.HashMap[String, String])
    }

    //left or right
    val side = line.split("_")(1)

    //pull out the primer position
    if (line.matches(REGEX_PREFIX + "=[0-9]*,[0-9]*")) {
      primerMap(primer) += (side + "_POSITION") -> line.split("=")(1)
    }
    else {
      //everything besides position looks the same - everything after the primer number is the
      //key, everything after the "=" is the value
      val pair = line.substring(line.indexOf("_" + primer) + 2 + primer.size).split("=")
      primerMap(primer) += side + "_" + pair(0) -> pair(1)
    }
}

println(formatOutput(outputType, primerMap))

def formatOutput(outputType: String, primerMap: Map[String, Map[String, String]]) = {
  if (outputType == "pgrep") {
    formatPgrepInput(primerMap)
  }
  else if (outputType == "table") {
    formatTabularOutput(primerMap)
  }
}

def formatTabularOutput(primerMap: Map[String, Map[String, String]]) = {
  val out = new StringBuffer()
  out.append(outputLine("PRIMER_SEQUENCE", "PRIMER_START_POSITION", "PRIMER_PENALTY", "PRIMER_TM",
    "PRIMER_GC_PERCENT", "PRIMER_SELF_ANY_TH", "PRIMER_SELF_END_TH", "PRIMER_HAIRPIN_TH", "PRIMER_END_STABILITY"))

  /*
  * The primer map is set up by primer number, and each entry contains data for a left primer and a right primer
  * For the purposes of this tabular output, the "left" and "right" part is irrelevant. Moreover, if input
  * was generated as a primer list by p3, it is also often wrong. Each entry, therefore, is treated as two
  * individual and unrelated primers
  */
  primerMap.keys.toList.sortBy(_.toInt).foreach {
    primer =>
      val data = primerMap(primer)
      out.append(extractTableLine("RIGHT", data))
      out.append(extractTableLine("LEFT", data))
  }
  out
}

def extractTableLine(side: String, data: Map[String, String]) = {
  val out = new StringBuffer()
  out.append(outputLine(data(side + "_SEQUENCE"), data(side + "_POSITION"), data(side + "_PENALTY"),
    data(side + "_TM"), data(side + "_GC_PERCENT"), data(side + "_SELF_ANY_TH"), data(side + "_SELF_END_TH"),
    data(side + "_HAIRPIN_TH"), data(side + "_END_STABILITY")))
  out
}

/**
 *
PRIMER_LEFT_9_PENALTY=0.039330
PRIMER_RIGHT_9_PENALTY=0.039330
PRIMER_LEFT_9_SEQUENCE=GTGAACCCAGTAGGAGCCATCTTCA
PRIMER_RIGHT_9_SEQUENCE=GAAGATGGCTCCTACTGGGTTCACA
PRIMER_LEFT_9=1346,25
PRIMER_RIGHT_9=1369,25
PRIMER_LEFT_9_TM=65.961
PRIMER_RIGHT_9_TM=65.961
PRIMER_LEFT_9_GC_PERCENT=52.000
PRIMER_RIGHT_9_GC_PERCENT=52.000
PRIMER_LEFT_9_SELF_ANY_TH=0.00
PRIMER_RIGHT_9_SELF_ANY_TH=0.00
PRIMER_LEFT_9_SELF_END_TH=0.00
PRIMER_RIGHT_9_SELF_END_TH=0.00
PRIMER_LEFT_9_HAIRPIN_TH=37.40
PRIMER_RIGHT_9_HAIRPIN_TH=34.36
PRIMER_LEFT_9_END_STABILITY=3.0200
PRIMER_RIGHT_9_END_STABILITY=3.5800
 */


def extractPrimerNumber(line: String) = line.split("_|=")(2)

def formatPgrepInput(primerMap: Map[String, Map[String, String]]) = {
  val out = new StringBuilder()
  out.append(outputLine("primer name", "forward sequence", "reverse sequence", "product size"))
  //go through primer by primer
  primerMap.keys.toList.sortBy(_.toInt).foreach {
    primer =>

      val data = primerMap(primer)
      val productSize = getProductSize(data)

      out.append(outputLine(primer + "_", data("RIGHT_SEQUENCE"), data("LEFT_SEQUENCE"), productSize))
  }
  out.toString
}

/**
 * Calculate product size from left and right primer locations when it's not supplied
 */
def getProductSize(primerEntry: Map[String, String]) = {
  if (primerEntry.contains("PRODUCT_SIZE")) {
    primerEntry("PRODUCT_SIZE")
  }
  else {
    throw new IllegalStateException("No product size in p3 output. Did you generate a primer " +
            "list instead of primer pairs?")
  }
}

/**
 * output a line in the pgrep input format
 * note that we must append a "_" to the primer name in order to avoid a bug in pgrep
 */
def outputLine(names: String*) = {
  val out = new StringBuilder();
  for (x <- 0.until(names.size)) {
    out.append(names(x))
    if (x != names.size - 1) out.append("\t")
  }
  out.append("\n")
  out.toString
}

def getInteger(s: String) = {
  try {
    s.toInt
  }
  catch {
    case _: java.lang.NumberFormatException => None
  }
}



