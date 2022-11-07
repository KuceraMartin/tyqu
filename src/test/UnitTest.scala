//> using lib "org.scalameta::munit::0.7.29"

package tyqu

import munit.FunSuite


abstract class UnitTest extends FunSuite:

	def assertContains(haystack: String, needles: String*): Unit =
		val errors = needles.filterNot(haystack.contains)
		if (errors.nonEmpty)
			fail(f"assertContains failed!\n\n$haystack\n\ndoes not contain\n\n" + errors.mkString("\n\n"))
