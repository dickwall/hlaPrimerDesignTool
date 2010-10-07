import com.locusdev.primer.Primer
import com.locusdev.snp.{CondensedSnp, SnpSearch}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers



/**
 * User: afurman
 * Date: Sep 16, 2010
 * Time: 3:24:13 PM
 * Copyright: Locus Development Inc
 */

class SnpSearchTest extends FunSuite with ShouldMatchers {
  //primer from 100 to 150
  val primer = Primer("GATGGGAGTCAGGGAAACTGTCCTT\t18758\t6.44E-4\t65.999\t52.0\t0.13\t0.0\t43.33\t3.36\tAPOE_03<19:45401006,45421219>\t19\t100\t150\treverse\t1\t0\t0\t0\t0\t0")
  
  test("middle"){
    SnpSearch.overlaps(new CondensedSnp("lalal", "chr1", 120, 121), primer) should be (1)
  }

  test("left"){
    SnpSearch.overlaps(new CondensedSnp("lalal", "chr1", 99, 100), primer) should be (1)
  }

  test("right"){
    SnpSearch.overlaps(new CondensedSnp("lalal", "chr1", 150, 170), primer) should be (1)
  }

  test("around"){
    SnpSearch.overlaps(new CondensedSnp("lalal", "chr1", 0, 170), primer) should be (1)
  }

  test("off left"){
    SnpSearch.overlaps(new CondensedSnp("lalal", "chr1", 0, 70), primer) should be (0)
  }

  test("off right"){
    SnpSearch.overlaps(new CondensedSnp("lalal", "chr1", 165, 170), primer) should be (0)
  }



}