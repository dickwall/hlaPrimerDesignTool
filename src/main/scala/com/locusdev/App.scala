package com.locusdev

import com.locusdev.hlatool._
import scala.collection._
import java.io.FileReader

/**
 * Hello world!
 *
 */
object App {
  def main(args: Array[String]) {
    val data = DbMhcParser.extractSequence(new FileReader("/Users/alexfurman/projects/locustools/src/main/resources/dbMHC_allelev2.28.xml"),
      "HLA-B", Array("Exon2", "Exon3"), 6)

    val haplotyper = new Haplotyper

    //val signature = data("B*0766")

    var list = combineSequence(data)

    val mutations = haplotyper.findAllMutations(list)

    val mmap = haplotyper.mutationMap(mutations)

    println("Answer: " + haplotyper.findAnswer(data, mmap, "B*1502"))

  }

  def combineSequence(alleles: Map[String, Map[String, String]]) = {
    val sequences =  mutable.ListBuffer[String]();

    sequences.toList
  }
}
