package com.locusdev.hlatool

import xml.XML
import scala.collection._

/**
 *
 * The dbMHC database provides an open, publicly accessible platform for DNA and clinical data related to the
 * human Major Histocompatibility Complex
 *
 * This is a simple parser/dataExtractor for the dbMHC XML format
 *
 * User: alexfurman
 * Date: Apr 8, 2010
 * Time: 11:21:40 AM
 */

object DbMhcParser {
  def extractSequence(reader: java.io.Reader, locus: String, dnaBlock: String) = {
    val start = System.currentTimeMillis;

    println("parsing dbHMC xml")
    val xml = XML load reader
    println("parsed dbHMC xml in " + ((System.currentTimeMillis - start) / 1000) + " seconds")

    val alleles = (xml \ "allele") filter {
      allele => (allele \ "locus" \ "name").text.equals(locus)
    }

    val data = new mutable.ListBuffer[(String, String)]

    for (allele <- alleles) {
      val name = (allele \ "name").text
      val blocks = allele \ "blocks" \ "block"
      for (block <- blocks) {
        val blockName = (block \ "name").text
        if (blockName.equals(dnaBlock)) {
          data += (name, (block \ "sequence").text)
        }
      }
    }

    data.toList
  }
}