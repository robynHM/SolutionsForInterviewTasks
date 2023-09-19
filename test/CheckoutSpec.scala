
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CheckoutSpec extends AnyWordSpec with Matchers {

  val testCheckout = new Checkout

  "Checkout" should {

    "checkoutDoubleList should calculate the sum of a list of apples and oranges" in {
      testCheckout.checkoutDoubleList(List(
        Shop.apple,
        Shop.orange
      )) shouldBe 0.85
    }

    "checkoutStringList should calculate the sum of a list of apples and oranges" in {
      testCheckout.checkoutStringList(List(
        "orange",
        "apple")
      ) shouldBe 0.85
    }

    "checkoutDoublesTotalBill should calculate the sum of a list of apples and oranges with no change in price when deals are not activated" in {
      testCheckout.checkoutDoublesTotalBill(List(
        Shop.apple,
        Shop.orange
      )) shouldBe 0.85
    }

    "checkoutDoublesTotalBill should calculate the sum of a list when the deals for apples and oranges are activated" in {
      testCheckout.checkoutDoublesTotalBill(List(
          Shop.apple,
          Shop.apple,
          Shop.orange,
          Shop.orange,
          Shop.orange
      )) shouldBe 1.10
    }

    "checkoutDoublesTotalBill should calculate the sum of a list when some deals for apples and oranges are activated and add on the others that are not redeeming the deals" in {
      testCheckout.checkoutDoublesTotalBill(
        List(
          Shop.apple,
          Shop.apple,
          Shop.apple,
          Shop.orange,
          Shop.orange,
          Shop.orange,
          Shop.orange,
          Shop.orange)
      ) shouldBe 2.20
    }

    "checkoutDoublesTotalBill should calculate the sum of a list when it contains bananas and there are less bananas than apples" in {
      testCheckout.checkoutDoublesTotalBill(
        List(
          Shop.apple,
          Shop.apple,
          Shop.apple,
          Shop.banana,
          Shop.orange,
          Shop.orange,
          Shop.orange)
      ) shouldBe 2.30
    }

    "checkoutDoublesTotalBill should calculate the sum of a list when it contains bananas and there are more bananas than apples" in {
      testCheckout.checkoutDoublesTotalBill(
        List(
          Shop.banana,
          Shop.banana,
          Shop.apple,
          Shop.banana,
          Shop.orange,
          Shop.orange,
          Shop.orange)
      ) shouldBe 1.50
    }

    "checkoutDoublesTotalBill should calculate the sum of a list when it contains bananas and there are equal bananas than apples" in {
      testCheckout.checkoutDoublesTotalBill(
        List(
          Shop.banana,
          Shop.apple,
          Shop.apple,
          Shop.banana,
          Shop.orange,
          Shop.orange,
          Shop.orange)
      ) shouldBe 1.70
    }

    "checkoutDoublesTotalBill should calculate the sum of a list using 2 for 1 for bananas when there are no apples" in {
      testCheckout.checkoutDoublesTotalBill(
        List(
          Shop.banana,
          Shop.banana,
          Shop.orange,
          Shop.orange,
          Shop.orange)
      ) shouldBe 0.70
    }

    "checkoutStringsTotalBill should calculate the sum of a list of apples and oranges with no change in price when deals are not activated" in {
      testCheckout.checkoutStringsTotalBill(List(
        "apple",
        "orange")) shouldBe 0.85
    }

    "checkoutStringsTotalBill should calculate the sum of a list when the deals for apples and oranges are activated" in {
      testCheckout.checkoutStringsTotalBill(List(
        "apple",
        "apple",
        "orange",
        "orange",
        "orange"
      )) shouldBe 1.10
    }

    "checkoutStringsTotalBill should calculate the sum correctly when strings are not all written in lowercase with no whitespaces" in {
      testCheckout.checkoutStringsTotalBill(List(
        "Apple",
        "APPLE",
        " orange",
        "orange ",
        "orAnge"
      )) shouldBe 1.10
    }

    "checkoutStringsTotalBill should calculate the sum of a list when some deals for apples and oranges are activated and add on the others that are not redeeming the deals" in {
      testCheckout.checkoutStringsTotalBill(
        List(
          "apple",
          "apple",
          "apple",
          "orange",
          "orange",
          "orange",
          "orange",
          "orange"
        )) shouldBe 2.20
    }

    "checkoutStringsTotalBill should calculate the sum of a list when it contains bananas and there are less bananas than apples" in {
      testCheckout.checkoutStringsTotalBill(
        List(
          "apple",
          "apple",
          "apple",
          "banana",
          "orange",
          "orange",
          "orange")
      ) shouldBe 2.30
    }

    "checkoutStringsTotalBill should calculate the sum of a list when it contains bananas and there are more bananas than apples" in {
      testCheckout.checkoutStringsTotalBill(
        List(
          "banana",
          "banana",
          "apple",
          "banana",
          "orange",
          "orange",
          "orange")
      ) shouldBe 1.50
    }

    "checkoutStringsTotalBill should calculate the sum of a list when it contains bananas and there are equal bananas than apples" in {
      testCheckout.checkoutStringsTotalBill(
        List(
          "banana",
          "apple",
          "apple",
          "banana",
          "orange",
          "orange",
          "orange")
      ) shouldBe 1.70
    }

    "checkoutStringsTotalBill should calculate the sum of a list using 2 for 1 for bananas when there are no apples" in {
      testCheckout.checkoutStringsTotalBill(
        List(
          "banana",
          "banana",
          "orange",
          "orange",
          "orange")
      ) shouldBe 0.70
    }

  }
}
