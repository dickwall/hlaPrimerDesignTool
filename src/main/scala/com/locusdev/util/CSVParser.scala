package com.locusdev.util

import collection._
import java.lang.String

/**
 * Simple parser for the CSV file format adapted from
 */

object CSVMapParser {
  def parse(lines: Iterator[String], separator: String): List[Map[String, String]] = parse(lines, separator, true)

  def parse(lines: Iterator[String], separator: String, strictLengths: Boolean): List[Map[String, String]] = {
    val results = new mutable.ListBuffer[Map[String, String]]

    if (!lines.hasNext) {
      throw new IllegalStateException("Passed in iterator is empty")
    }

    //the first line in the iterator is the header line
    val headerTokens: Array[String] = lines.next.split(separator)

    var counter = 0;
    while (lines.hasNext) {
      counter += 1
      val tokens = lines.next.split(separator);

      val map = new mutable.HashMap[String, String]

      if (strictLengths && tokens.length != headerTokens.length) {
        throw new IllegalStateException("Tokens mismatch: header has " + headerTokens.length + " tokens while line " +
                counter + " has " + tokens.length + " tokens");
      }

      for (x <- 0 until tokens.length) {
        map += headerTokens(x) -> tokens(x)
      }

      results += map
    }


    results.toList
  }
}
