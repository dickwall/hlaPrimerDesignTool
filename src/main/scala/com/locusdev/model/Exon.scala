package com.locusdev.model

/**
 * Simple exon definition
 *
 * User: afurman
 * Date: Aug 20, 2010
 * Time: 11:15:09 AM
 */

class Exon(val name: String, val chromosome: String, val start: Int, val end: Int, val distanceToNextExon: Option[Int], val notes: Option[String]){
  val length = end - start;

  override def toString = {
    "Exon[crom " + chromosome + ": " + start + ", " + end + "]"
  }
}