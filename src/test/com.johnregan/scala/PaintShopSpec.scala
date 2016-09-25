import org.scalatest.{FlatSpec, Matchers}

class PaintShopSpec extends FlatSpec with Matchers {

  "PaintShop" should "handle a single base case Matte" in {

    val result = PaintShop.solve("initialExample.txt")

    result shouldEqual "G G G G M"
  }

  it should "handle a multiple base case Matte" in {

    val result = PaintShop.solve("richerExample.txt")

    result shouldEqual "G M G M G"
  }

  it should "In two differing orders, at least one paint should be satisfied" in {

    val result = PaintShop.solve("finalExample.txt")

    result shouldEqual "M M"
  }

  it should "fail" in {

    val result = PaintShop.solve("noSolution.txt")

    result shouldEqual "No solution exists"
  }
}