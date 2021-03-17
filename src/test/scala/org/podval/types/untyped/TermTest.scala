package org.podval.types.untyped

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class TermTest extends AnyFlatSpec with Matchers {
  import Term._

  "parse()" should "work" in {
    def check(string: String): Unit = parse(string).toString shouldBe string

    check("λxy.x"   )
    check("(λx.xy)z")

    check("tuv")
    check("t(uv)"   )
    check("(λx.x)y" )

    parse("(tu)v"   ).toString shouldBe "tuv"
    parse("λx.(xy)" ).toString shouldBe "λx.xy"
  }

  "FV() and isClosed()" should "work" in {
    def check(string: String, free: Set[VariableName], closed: Boolean): Unit = {
      val term: Term = parse(string)
      FV(term) shouldBe free
      isClosed(term) shouldBe closed
    }

    check("(λx.xy)z", Set('y', 'z'), closed = false)
    check("λxy.x", Set.empty, closed = true)
  }
}
