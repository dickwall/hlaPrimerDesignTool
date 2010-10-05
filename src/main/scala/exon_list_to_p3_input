#!/bin/sh
exec scala "$0" "$@"
!#


import com.locusdev.model.Exon
import com.locusdev.primer.{P3Parser, ExonListParser}
import java.io.{FileWriter, PrintWriter, File}

/**
 * Takes an exon list and dumps out a p3 input file per exon
 */

if (args.length < 2) {
  println("Usage: exon_list_to_p3_input.scala exonList chromosomeDir outputDir")
  System.exit(-1)
}

val exons = ExonListParser.parseExonList(args(0))

val results = P3Parser.createP3Input(new File(args(1)), exons, 10, 10000)

results.keys.foreach {
  exon: Exon =>

  printToFile(results(exon), exon.name, args(2))
}


def printToFile(content: String, name: String, outputDir: String){
  val out = new PrintWriter(new FileWriter(new File(outputDir, name + ".p3.input.txt")))
  out.print(content)
  out.close
}
