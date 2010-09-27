#!/bin/sh
exec scala "$0" "$@"
!#


import com.locusdev.snp.SnpParser
import java.lang.String
import io.Source
import java.io.File
/**
 * Quick script to do some statistics on the SNPS131 data file. Decided to do things this way and not
 * through SQL to save time in the short-term. Will likely need a more permanent solution for this sort
 * of thing in the future.
 *
 * This script is an Abomination to the Lord. Fortunately, we're not likely to ever need to reuse it.
 */

val fileName = if (args.size > 0) args(0) else "";
if (fileName == "") {
  println("Usage: snps_131_summary.scala file")
  System.exit(0)
}

//make sure the file exists
val file = new File(fileName)

if (!file.exists || file.isDirectory) {
  println(file.getAbsolutePath + " does not exist or is a directory")
}

//Convert lines to list of snps
val snps = SnpParser.parse(Source.fromFile(file).getLines())

println("Filtering SNPS")

//counters
var notGenomic = 0
var noChrom = 0
var chromWeird = 0
var count = 0;

val weirdChroms = new collection.mutable.HashSet[String]

val lengthMap = new collection.mutable.HashMap[Int, Int]
val lengthExamplesMap = new collection.mutable.HashMap[Int, String]

val classMap = new collection.mutable.HashMap[String, Int]
val classByLengthMap = new collection.mutable.HashMap[String, collection.mutable.Map[Int, Int]]
val classByValidsMap = new collection.mutable.HashMap[String, collection.mutable.Map[Set[String], Int]]

val validsPerSnp = new collection.mutable.HashMap[Int, Int]
val snpsPerValid = new collection.mutable.HashMap[String, Int]
var freqAndCluster = 0

val weight = new collection.mutable.HashMap[Int, Int]
weight += 1 -> 0
weight += 2 -> 0
weight += 3 -> 0

for (snp <- snps) {
  count += 1

  if (count % 100000 == 0) {
    println("filtered snps " + count)
  }

  if (snp.molType != "genomic") {
    notGenomic += 1
  }
  else if (snp.chrom == null || snp.chrom.trim.length == 0) {
    noChrom += 1
  }
  else if (snp.chrom.length != 4 && snp.chrom.length != 5) {
    weirdChroms += snp.chrom
    chromWeird += 1
  }
  else {
    //now, keep track of all the stats we need

    //first, length
    handleLength(lengthMap)

    //now, class
    if (classMap.contains(snp.clazz)) {
      classMap += snp.clazz -> (classMap(snp.clazz) + 1)
    }
    else {
      classMap += snp.clazz -> 1
    }

    //now, length by class

    if (!classByLengthMap.contains(snp.clazz)) {
      classByLengthMap += snp.clazz -> new collection.mutable.HashMap[Int, Int]
      classByValidsMap += snp.clazz -> new collection.mutable.HashMap[Set[String], Int]
    }

    val validsMapForClass = classByValidsMap(snp.clazz)

    if(!validsMapForClass.contains(snp.valid)){
      validsMapForClass += snp.valid -> 1
    }
    else{
      val i: Int = validsMapForClass(snp.valid) + 1
      validsMapForClass += snp.valid -> i
    }

    val lbc = classByLengthMap(snp.clazz)
    handleLength(lbc)

    //now, count up all the "valids"
    val valid = snp.valid

    if (validsPerSnp.contains(valid.size)) {
      validsPerSnp += valid.size -> (validsPerSnp(valid.size) + 1)
    }
    else {
      validsPerSnp += valid.size -> 1
    }

    //go through all valids and increment them
    valid.foreach {
      v =>
        if (snpsPerValid.contains(v)) {
          snpsPerValid += v -> (snpsPerValid(v) + 1)
        }
        else {
          snpsPerValid += v -> 1
        }
    }

    //check for both freq and cluster
    if (valid.contains("by-cluster") && valid.contains("by-frequency")) {
      freqAndCluster += 1
    }

    //weights
    weight += snp.weight -> (weight(snp.weight) + 1)

  }

  def handleLength(map: collection.mutable.Map[Int, Int]) = {
    val length = snp.chromEnd - snp.chromStart
    if (map.contains(length)) {
      map += length -> (map(length) + 1)
    }
    else {
      map += length -> 1
    }

    if (!lengthExamplesMap.contains(length)) {
      lengthExamplesMap += length -> snp.name
    }
  }
}

println("Not Genomic: " + notGenomic)
println("Not Chromosome Location: " + noChrom)
println("Chrom Weird: " + chromWeird)
println("Weird Cromosomes: " + weirdChroms)
println("Total Processed: " + count)

println("\n\n")

//print out all the lengths w/ examples
println("Length\tCount\tExample")
lengthMap.keys.toList.sort((a, b) => a < b).foreach {
  length =>
    print(length)
    print("\t")
    print(lengthMap(length))
    print("\t")
    println(lengthExamplesMap(length))

}

println("\n\n")

println("CLASSES\n")
//print class map
println("Class\tCount")
classMap.keys.toList.sort((a, b) => a < b).foreach {
  length =>
    print(length)
    print("\t")
    println(classMap(length))
}

println("\n\n")

//length by class
classByLengthMap.keys.toList.sort((a, b) => a < b).foreach {
  clazz =>
    val l4class = classByLengthMap(clazz)
    println("\nCLASS: " + clazz)
    println("\nLength\tCount")
    l4class.keys.toList.sort((a, b) => a < b).foreach {
      length =>
        print(length)
        print("\t")
        println(l4class(length))
    }

}

println("\n\n")
println("#VALIDS\t#SNPS")
validsPerSnp.keys.toList.sort((a, b) => a < b).foreach {
  valids =>
    print(valids)
    print("\t")
    println(validsPerSnp(valids))
}


println("\n\n")
println("VALID\t#SNPS")
snpsPerValid.keys.toList.sort((a, b) => a < b).foreach {
  valids =>
    print(valids)
    print("\t")
    println(snpsPerValid(valids))
}

//length by class
println("\n\nVALIDS BY CLASS\n")
classByValidsMap.keys.toList.sort((a, b) => a < b).foreach {
  clazz =>
    val v4class = classByValidsMap(clazz)
    println("\nCLASS: " + clazz)
    println("\nValids\tCount")
    v4class.keys.toList.sort((a, b) => a.toString < b.toString).foreach {
      valids =>
        print(valids)
        print("\t")
        println(v4class(valids))
    }

}


println("\n\n")
println("Valid By Frequency And Cluster: " + freqAndCluster)

println("\n\n")
println("Weights: " + weight)



