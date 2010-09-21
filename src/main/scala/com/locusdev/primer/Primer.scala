package com.locusdev.primer

/**
 *
 *
 * User: afurman
 * Date: Sep 16, 2010
 * Time: 1:12:55 PM
 * Copyright: Locus Development Inc
 */

class Primer(val sequence: String, val blockStartPosition: Int, val penalty: Double, val tm: Double, val gcPercent: Double,
             val selfAnyTh: Double, val selfEndTh: Double, val hairpinTh: Double, val endStability: Double, val exon: String, val chromosome: String,
             val startPosition: Int, val endPosition: Int, val direction: String, val perfectMatches: Option[Int], val nearMatches: Option[Int], val anyShortSnpHits:
Option[Int], val anyLongSnpHits: Option[Int], val realShortSnpHits: Option[Int], val realLongSnpHits: Option[Int]) {
  override def toString = "Primer[chrom " + chromosome + ": " + startPosition + "," + endPosition + "]"

  /**
   * Returns a new Primer with the snp hits filled out. The original Primer remains unchanged
   */
  def setSnpHits(anyShortHits: Int, anyLongHits: Int, realShortHits: Int, realLongHits: Int) = {
    //blat hits must be set lest we screw up formatting
    new Primer(sequence, blockStartPosition, penalty, tm, gcPercent, selfAnyTh, selfEndTh, hairpinTh, endStability, exon,
      chromosome, startPosition, endPosition, direction, perfectMatches, nearMatches,
      Option(anyShortHits), Option(anyLongHits), Option(realShortHits), Option(realLongHits))
  }

  def blatHitsSet = nearMatches != None && perfectMatches != None

  def snpHitsSet = anyShortSnpHits != None && anyLongSnpHits != None && realShortSnpHits != None && realLongSnpHits != None

  def toTableLine = {
    require(!snpHitsSet || blatHitsSet, "Cannot output table line if snp hits are set and blat hits are not")
    val main = List(sequence, blockStartPosition, penalty, tm, gcPercent, selfAnyTh, selfEndTh, hairpinTh, endStability, exon, chromosome,
      startPosition, endPosition, direction).mkString("\t")

    val blat = List(perfectMatches.get, nearMatches.get).mkString("\t")
    val snp = List(anyShortSnpHits.get, anyLongSnpHits.get, realShortSnpHits.get, realLongSnpHits.get).mkString("\t")

    if (!blatHitsSet) main
    else if (!snpHitsSet) main + "\t" + blat
    else main + "\t" + blat + "\t" + snp
  }
}

object Primer {
  def apply(primerCsv: String) = {
    val tokens = primerCsv.split("\t")

    //valid token lengths depending on which state the primer table is in 14 for nw, 16 for post-blat, and 20 for post-snp-filter
    require(tokens.length == 14 || tokens.length == 16 || tokens.length == 20, "Unexpected primer line: " + primerCsv)

    //optional blat fields
    val perfectMatches = if (tokens.length > 14) Option(tokens(14).toInt) else None
    val nearMatches = if (tokens.length > 14) Option(tokens(15).toInt) else None

    //optional snp filter fields
    val anyShortHits = if (tokens.length > 16) Option(tokens(16).toInt) else None
    val anyLongHits = if (tokens.length > 16) Option(tokens(17).toInt) else None
    val realShortHits = if (tokens.length > 16) Option(tokens(18).toInt) else None
    val realLongHits = if (tokens.length > 16) Option(tokens(19).toInt) else None


    new Primer(tokens(0), tokens(1).toInt, tokens(2).toDouble, tokens(3).toDouble, tokens(4).toDouble, tokens(5).toDouble,
      tokens(6).toDouble, tokens(7).toDouble, tokens(8).toDouble, tokens(9), tokens(10), tokens(11).toInt,
      tokens(12).toInt, tokens(13), perfectMatches, nearMatches, anyShortHits, anyLongHits, realShortHits, realLongHits)

  }

}