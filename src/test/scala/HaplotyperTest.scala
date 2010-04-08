import com.locusdev.hlatool.{Mutation, Haplotyper}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test for the Haplotyper class
 */

class HaplotyperTest extends FunSuite with ShouldMatchers {

  val s1 = "gattacca"
  val s2 = "gatcatca"
  val s3 = "gat-acca"
  val s4 = "gattaca"

  val s5 = "cga"
  val s6 = "caa"
  val s7 = "aga"
  val s8 = "cgc"

  val haplotyper = new Haplotyper

  test ("Should not compare different length sequences") {
    intercept[IllegalArgumentException] {
      haplotyper.extractMutationSets(s1, s4)
    }
  }

  test ("Should return sets of mutations from pair of strings") {
    val m1 = haplotyper.extractMutationSets(s1, s2)
    val m2 = haplotyper.extractMutationSets(s1, s3)

    m1 should be (Array(Set('g'), Set('a'), Set('t'), Set('t','c'), Set('a'), Set('c','t'), Set('c'), Set('a')))
    m2 should be (Array(Set('g'), Set('a'), Set('t'), Set('t','-'), Set('a'), Set('c'), Set('c'), Set('a')))
  }

  test ("Should return sets of mutations from list of sequences") {
    val m1 = haplotyper.findAllMutations(List(s1, s2, s3))

    m1 should be (Array(Set('g'), Set('a'), Set('t'), Set('t','c','-'), Set('a'), Set('c','t'), Set('c'), Set('a')))
  }

  test ("Should not compare list of sequences where some of the sequences differ in length") {
    intercept[IllegalArgumentException] {
      haplotyper.findAllMutations(List(s1, s2, s3, s4))
    }
  }

  test ("Should return map of mutations only for mutable alleles - odd number of sequences") {
    val m1 = haplotyper.findAllMutations(List(s1, s2, s3))
    val mutationMap = haplotyper.mutationMap(m1)

    mutationMap should be (Map(3 -> Set('t','c','-'), 5 -> Set('c', 't')))
  }

  test ("Should return map of mutations only for mutable alleles - even number of sequences") {
    val m1 = haplotyper.findAllMutations(List(s1, s2))
    val mutationMap = haplotyper.mutationMap(m1)

    mutationMap should be (Map(3 -> Set('t','c'), 5 -> Set('c', 't')))
  }

  test ("Should return map of mutations only for mutable alleles - only one sequences") {
    val m1 = haplotyper.findAllMutations(List(s1))
    val mutationMap = haplotyper.mutationMap(m1)

    // should result in empty mutation map
    mutationMap should be (Map[Int, Set[Char]]())
  }

  test ("Should return map of mutations only for mutable alleles - empty sequence list") {
    val m1 = haplotyper.findAllMutations(List[String]())
    val mutationMap = haplotyper.mutationMap(m1)

    // should result in empty mutation map
    mutationMap should be (Map[Int, Set[Char]]())
  }

  test ("Should get correct haplotype combination back from test sequence") {
    val m1 = haplotyper.findAllMutations(List(s1, s2, s3))
    val mutationMap = haplotyper.mutationMap(m1)

    val haplotype = haplotyper.getHaplotype("gatcacca", mutationMap)

    haplotype should be (List(Mutation(3,'c'), Mutation(5,'c')))
  }

  test ("Find Signatures for cgc") {
    val m1 = haplotyper.findAllMutations(List(s5, s6, s7, s8))
    val mutationMap = haplotyper.mutationMap(m1)

    val signatures = haplotyper.findAllUniqueSignatures(s8, List(s5, s6, s7), mutationMap)

    println(signatures)
  }

  test ("Find Signatures for cga with only 3 items") {
    val m1 = haplotyper.findAllMutations(List(s5, s6, s7))
    val mutationMap = haplotyper.mutationMap(m1)

    val signatures = haplotyper.findAllUniqueSignatures(s5, List(s6, s7), mutationMap)

    println(signatures)
  }

}
