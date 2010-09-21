package com.locusdev.snp


/**
 * Created by IntelliJ IDEA.
 * User: afurman
 * Date: Sep 13, 2010
 * Time: 12:24:51 PM
 * To change this template use File | Settings | File Templates.
 */

object SnpParser {
  def parse(lines: Iterator[String]):Iterator[Snp] = {
    lines.map(parse(_))
  }

  def parse(line: String): Snp = {
    val tokens = line.split("\t");
    val valid = tokens(COL_VALID).split(",").toList.map(_.trim).toSet;
    val func = tokens(COL_FUNC).split(",").toSet;

    new Snp(tokens(COL_CHROM), tokens(COL_CHROM_START).toInt, tokens(COL_CHROM_END) toInt, tokens(COL_NAME),
      tokens(COL_STRAND)(0), tokens(COL_REF_NCBI), tokens(COL_REF_UCSC), tokens(COL_OBSERVED), tokens(COL_MOL_TYPE),
      tokens(COL_CLASS), valid, tokens(COL_AV_HET).toDouble, tokens(COL_AV_HET_SE).toDouble, func,
      tokens(COL_LOC_TYPE), tokens(COL_WEIGHT).toInt)
  }


  //define the columns in order
  val COL_CHROM = 1
  val COL_CHROM_START = 2
  val COL_CHROM_END = 3
  val COL_NAME = 4
  // "score" is an unused column
  val COL_STRAND = 6
  val COL_REF_NCBI = 7
  val COL_REF_UCSC = 8
  val COL_OBSERVED = 9
  val COL_MOL_TYPE = 10
  val COL_CLASS = 11
  val COL_VALID = 12
  val COL_AV_HET = 13
  val COL_AV_HET_SE = 14
  val COL_FUNC = 15
  val COL_LOC_TYPE = 16
  val COL_WEIGHT = 17


}

