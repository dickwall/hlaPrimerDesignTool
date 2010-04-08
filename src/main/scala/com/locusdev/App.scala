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
      "HLA-B", "Exon3")

    println(data)
  }
}
