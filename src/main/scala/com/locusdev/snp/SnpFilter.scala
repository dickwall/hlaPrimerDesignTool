package com.locusdev.snp

import java.io.File
import com.locusdev.util.CSVParser
import io.Source

/**
 * Created by IntelliJ IDEA.
 * User: afurman
 * Date: Sep 14, 2010
 * Time: 10:23:53 AM
 * To change this template use File | Settings | File Templates.
 */

class SnpFilter(rules: List[SnpFilterRule]) {

  //primer the matches table
  rules.foreach(SnpFilter.matchCounts += _ -> 0)

  /**
   * A snp is considered to have passed the filter as long as there's at least one rule that matches it
   */
  def matches(snp: Snp) = {

    val matched = rules.filter(_.matches(snp))
    require(matched.length <= 1, snp + " matched more than one rule: " + matched)

    if (!matched.isEmpty) {
      val rule = matched(0)
      SnpFilter.matchCounts += rule -> (SnpFilter.matchCounts(rule) + 1)
    }

    !matched.isEmpty
  }
}


object SnpFilter {
  val matchCounts = new collection.mutable.HashMap[SnpFilterRule, Int]
  var checkedHetRules = 0;

  /**
   * Creates a snp filter from a rules file in CSV format
   */
  def apply(rulesFile: File) = {
    require(rulesFile.exists, "Filter rules file does not exist: " + rulesFile.getAbsolutePath)
    val tokens = CSVParser.parse(Source.fromFile(rulesFile).getLines, "\t")
    val rules = tokens.map(ruleFromTokens(_))
    new SnpFilter(rules)
  }

  def ruleFromTokens(tokens: scala.collection.Map[String, String]) = {
    def requireColumn(tokens: scala.collection.Map[String, String], column: String) = require(tokens.contains(column), "Missing required column: " + column + ", got: " + tokens.mkString(", "))
    val COL_CLASS = "Class"
    val COL_VALID = "Valid Status"
    val COL_HET = "Het"

    //validate input
    requireColumn(tokens, COL_CLASS)
    requireColumn(tokens, COL_VALID)
    requireColumn(tokens, COL_HET)

    val classes = tokens(COL_CLASS).split(",").map(_.trim).toSet
    val valids = tokens(COL_VALID).split(",").map(_.trim).toSet
    val hetRules = tokens(COL_HET).split(",").map(_.trim).map {
      hetString =>
        if (hetString == "Any") new Any()
        else if (hetString == "NOTVENTER") new NotVenter()
        else if (hetString.startsWith(">")) new GreaterThan(hetString.substring(1).toDouble)
        else {
          throw new IllegalArgumentException("Illegal Het string: " + hetString)
        }
    }.toSet

    new SnpFilterRule(classes, valids, hetRules)
  }

  def main(args: Array[String]) {
    SnpFilter(new File(args(0)))
  }
}


class SnpFilterRule(val classes: Set[String], val validStatus: Set[String], val hetRules: Set[ScalaObject with HetRule]) {
  /**
   * Returns whether or not a snp passes the filter rule
   */
  def matches(snp: Snp): Boolean = {
    //does this filter apply to the snp's class? A snp matches if its class is included in the filter's set of classes
    if (!classes.contains(snp.clazz)) {
      return false
    }

    //now the validation statuses. a snp matches if its set of validation statuses is exactly the same as the rule's
    val validsMatch = validStatus.contains("Any") || (validStatus.equals(snp.valid))


    if (!validsMatch) {

      return false
    }

    //println("valids matched, het rules: " + hetRules)


    SnpFilter.checkedHetRules += 1

    //now, check het rules
    return !hetRules.map(_.passes(snp.avHet)).contains(false)
  }

  override def toString = "Classes[" + classes.mkString(",") + "] Valids[" + validStatus.mkString(",") + "] HetRules[" + hetRules.mkString(",") + "]"
}

trait HetRule {
  def passes(avHet: Double): Boolean
}

/**
 * Any het is fine
 */
class Any extends HetRule {
  override def passes(avHet: Double) = true

  override def toString = "Any"
}

/**
 * Average het has to be greater than a certain value
 */
class GreaterThan(val value: Double) extends HetRule {
  override def passes(avHet: Double) = avHet > value

  override def toString = ">" + value
}

/**
 * Average het cannot be a Venter Number (.625, .5, .48, .46875, .44, .375, 0.277778)
 *
 * What is a Venter Number or a Watson Number ?
 * Some people have had there genomes sequenced, such as Venter and Watson
 * Because they have a different base at some positions this are SNPs that have been submitted to  dbSNP
 * In some ases the only frequency data on a SNP is from Venter or Watson, or both
 * Or worse yet it is from venter being sequenced multiple times under different names or methods or whatever.
 * The results there are SNPs that have been seen in one person, but have frequencies that make them appear like more than that
 * Due to some glitch Venter Frequencies, HapMap Frequencies and 1000 genome frequencies are not combined, instead they often only list the Venter number
 * A large number of SNPs that are just venter or watson fall in to the categories "by-cluster,by-frequency, or just by-cluster"
 * SNPs in by-cluster,by-frequency,by-1000genomes are typically real and have more real SNPs than venter SNPs
 * If you look at the ratio of counts for a given frequency you can find bins with way too many SNPs, (see ratio column)
 * By defination I call venter SNPs when the Ratio is  > .5 and there are greater than 100 snps
 * Low Freq SNPs (< 3%) with high ratio numbers are actually more due to 1 SNP in a odd population number.

 *
 *
 */
class NotVenter extends HetRule {
  val venterNumbers = Set(0.625, 0.5, 0.48, 0.46875, 0.44, 0.375, 0.277778)

  override def passes(avHet: Double) = !venterNumbers.contains(avHet)

  override def toString = "NOTVENTER"
}