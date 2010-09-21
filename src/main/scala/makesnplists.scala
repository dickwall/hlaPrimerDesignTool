import com.locusdev.snp.{Snp, SnpFilter, SnpParser}
import io.Source
import collection._
import java.io.{FileWriter, PrintWriter, File}

/**
 * Script that takes in a dbsnp flat file of all the snps and filters them into two lists "any snps" and "real snps"
 * according to rules as specified by Mat. These lists are later used in primer design.
 */

if (args.length != 3) {
  println("usage: make_snp_lists input_file snp_rules_file output_dir")
  println("\tinput_fule is a snp file in dbsnp flat format. furrently we're using snps 131")
  println("\tsnp_rules_file is a csv file containing rules that define real snps")
  System.exit(-1)
}

val input = new File(args(0))
val rules = new File(args(1))
val outDir = new File(args(2))

require(input.exists, "input file does not exist: " + input.getAbsolutePath)
require(rules.exists, "rules file does not exist: " + rules.getAbsolutePath)
require(outDir.exists && outDir.isDirectory, "output dir does not exist or is not a dir: " + outDir.getAbsolutePath)

//read in all the snps
val lines = Source.fromFile(input).getLines

val writers = new mutable.HashMap[String, PrintWriter]()
def printToFile(list: String, snp: Snp, line: String) {
  val length = if (snp.length <= 1) "short" else "long"
  val printerId = list + "_" + snp.chrom + "_" + length + ".txt"

  if (!writers.contains(printerId)) {
    writers += printerId -> new PrintWriter(new FileWriter(new File(outDir, printerId)))
  }

  writers(printerId).println(line)
}


val filter = com.locusdev.snp.SnpFilter(rules)

var counter = 0;
var anyCount = 0;
var realCount = 0;

lines.foreach {
  line =>
    counter += 1;

    if (counter % 1000000 == 0) {
      println("Snps filtered:\t" + counter)
      println("Any:\t" + anyCount)
      println("Real:\t" + realCount)
    }
    val snp = SnpParser.parse(line)

    //are we in "AnySnps"
    val genomic = snp.molType == "genomic"
    val chromosomeValid = snp.chrom.length == 4 || snp.chrom.length == 5
    val weightOk = snp.weight == 1
    val notHet = snp.clazz != "het"


    if (genomic && chromosomeValid && weightOk && notHet) {

      //write this line to "any snps"
      val tokens = line.split("\t")
      val condensed = List(tokens(1), tokens(2), tokens(3), tokens(4), tokens(6)).mkString("\t")
      printToFile("any", snp, condensed)
      anyCount += 1

      if (filter.matches(snp)) {
        printToFile("real", snp, condensed)
        realCount += 1
      }

    }
}

writers.values.foreach(_.close())

println("DONE\n\n")
println("Snps filtered:\t" + counter)
println("Any:\t" + anyCount)
println("Real:\t" + realCount)

println("\n\n")

println("Checked het rules: " + SnpFilter.checkedHetRules)

SnpFilter.matchCounts.keys.foreach {
  rule =>
    println(rule + "\t" + SnpFilter.matchCounts(rule))
}

