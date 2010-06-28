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
import scala.io._
import scala.collection._

//get lines from stdin
val lines = Source.fromInputStream(System.in).getLines(compat.Platform.EOL);


val primerMap = new mutable.HashMap[String, Map[String, String]]

lines.foreach {
  line =>
  //println(line)
  //all the lines that we care about (the ones that actually describe primers) are int he following
  //format PRIMER_*_NUMBER_** where * can either be (RIGHT, LEFT, or PAIR) and ** is the actual
  //property. Any lines that don't follow this format can be ignored.
    val tokens = line.split("_")

    if (tokens.length > 3 && getInteger(tokens(2)) != None) {
      val primer = tokens(2)

      if (!primerMap.contains(primer)) {
        //prime the map if this is the first entry for our primer
        primerMap.put(primer, new mutable.HashMap[String, String])
      }

      if (tokens(3).startsWith("SEQUENCE")) {
        val subtokens = tokens(3).split("=")
        primerMap(primer) += tokens(1) + "_" + subtokens(0) -> subtokens(1)
      }
      else if (tokens(3).equals("PRODUCT")) {
        val subtokens = tokens(4).split("=")
        primerMap(primer) += "PRODUCT_SIZE" -> subtokens(1)
      }
    }

}


//now we have our map and need to output
outputHeader()
primerMap.keys.toList.sortBy(_.toInt).foreach {
  primer =>

  val data = primerMap(primer)

  outputLine(primer, data("RIGHT_SEQUENCE"), data("LEFT_SEQUENCE"), data("PRODUCT_SIZE"))
}


def outputHeader() = {
  outputLine("primer name", "forward sequence", "reverse sequence", "product size")
}

/**
 * output a line in the pgrep input format
 * note that we must append a "_" to the primer name in order to avoid a bug in pgrep
 */
def outputLine(name: String, forward: String, reverse: String, size: String) = {
  println(name + "_\t" + forward + "\t" + reverse + "\t" + size)
}

def getInteger(s: String) = {
  try {
    s.toInt
  }
  catch {
    case _: java.lang.NumberFormatException => None
  }
}



