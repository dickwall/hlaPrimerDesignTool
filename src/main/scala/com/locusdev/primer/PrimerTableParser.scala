package com.locusdev.primer

/**
 *
 *
 * User: afurman
 * Date: Sep 22, 2010
 * Time: 1:11:01 PM
 * Copyright: Locus Development Inc
 */

object PrimerTableParser {
  def parse(lines: Iterator[String]) = {
    val header = lines.next
    require(header startsWith "PRIMER_SEQUENCE", "Unexpected header: " + header)
    lines.map(Primer(_))
  }
}