package com.locusdev.snp

/**
 * A SNP as represented in the UCSC Genome browser
 * User: alexfurman
 * Date: Aug 5, 2010
 * Time: 1:18:33 PM
 */

class Snp(val chrom: String, val chromStart: Int, val chromEnd: Int, val name: String, val strand: Char,
          val refNCBI: String, val refUCSC: String,
          val observed: String, val molType: String, val clazz: String, val valid: Set[String], val avHet: Double,
          val avHetEr: Double,
          val func: Set[String], val locType: String, val weight: Int){
  override def toString() = "Snp [" + name + ", " + chrom + ": " + chromStart + "," + chromEnd + "]"
  def length = chromEnd - chromStart

}