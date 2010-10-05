#!/bin/sh
exec scala "$0" "$@"
!#

import io.Source
import java.lang.String

/**
 * Utility that converts a primer table file into a blattable fasta file
 *
 */

//get lines from stdin
val lines = Source.fromInputStream(System.in).getLines();

//parse out the primers. this is made easier
val firstLine: String = lines.next
if (!firstLine.startsWith("PRIMER_SEQUENCE")) {
  println("Unexpected first line: " + firstLine)
}

//loop through the lines and print out in fasta (the name of each primer is it's unique sequence, so this is trivial)
lines.foreach {
  line =>
    if (line.trim.length > 0) {
      val tokens = line.split("\t")
      println(">" + tokens(0))
      println(tokens(0))
    }
}

//we're done