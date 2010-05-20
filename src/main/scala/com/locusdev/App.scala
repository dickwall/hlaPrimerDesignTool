package com.locusdev

import com.locusdev.hlatool._
import java.io.FileReader

/**
 * Hello world!
 *
 */
object App {
  def main(args: Array[String]) {
    val data = DbMhcParser.extractSequence(new FileReader("/Users/alexfurman/projects/hlaPrimerDesignTool/src/main/resources/dbMHC_allelev2.28.xml"),
      "HLA-B", Array("Exon2", "Exon3"), 6)

    val haplotyper = new Haplotyper

    val signature = data("B*0766")

    var list = data.values.toList

    val mutations = haplotyper.findAllMutations(list)

    for(signature <- haplotyper.findAllUniqueSignatures(signature, list - signature, haplotyper.mutationMap(mutations))){
      println(signature.length + ": " + signature)
    }

    println(data)
  }
}
