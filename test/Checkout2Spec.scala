import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Checkout2Spec extends AnyWordSpec with Matchers {
  val testCheckout = new Checkout2

  "Checkout2" should {

    "printAndCalculateBill should calculate the sum of a list of apples and oranges" in {
      testCheckout.printAndCalculateBill(List(
        ShopItem(item = Apple, quantity = 1),
        ShopItem(item = Orange, quantity = 1)
      )) shouldBe "Apples: £0.60\nOranges: £0.25\nDiscounts Applied: £0.00\nTotal to pay: £0.85"
    }

    "printAndCalculateBill should calculate the sum of a list when the deals for apples and oranges are activated" in {
      testCheckout.printAndCalculateBill(List(
        ShopItem(item = Apple, quantity = 2),
        ShopItem(item = Orange, quantity = 3)
      )) shouldBe "Apples: £1.20\nOranges: £0.75\nDiscounts Applied: £0.85\nTotal to pay: £1.1"
    }

    "printAndCalculateBill should calculate the sum of a list when some deals for apples and oranges are activated and add on the others that are not redeeming the deals" in {
      testCheckout.printAndCalculateBill(
        List(
          ShopItem(item = Apple, quantity = 3),
          ShopItem(item = Orange, quantity = 5)
          )
      ) shouldBe "Apples: £1.80\nOranges: £1.25\nDiscounts Applied: £0.85\nTotal to pay: £2.2"
    }

    "printAndCalculateBill should calculate the sum of a list when it contains bananas and there are less bananas than apples" in {
      testCheckout.printAndCalculateBill(
        List(
          ShopItem(item = Apple, quantity = 3),
          ShopItem(item = Orange, quantity = 3),
          ShopItem(item = Banana, quantity = 1)
         )
      ) shouldBe "Apples: £1.80\nOranges: £0.75\nBananas: £0.20\nDiscounts Applied: £0.45\nTotal to pay: £2.3"
    }

    "printAndCalculateBill should calculate the sum of a list when it contains bananas and there are more bananas than apples" in {
      testCheckout.printAndCalculateBill(
        List(
          ShopItem(item = Apple, quantity = 1),
          ShopItem(item = Orange, quantity = 3),
          ShopItem(item = Banana, quantity = 3)
          )
      ) shouldBe "Apples: £0.60\nOranges: £0.75\nBananas: £0.60\nDiscounts Applied: £0.45\nTotal to pay: £1.5"
    }

    "printAndCalculateBill should calculate the sum of a list when it contains bananas and there are equal bananas than apples" in {
      testCheckout.printAndCalculateBill(
        List(
          ShopItem(item = Apple, quantity = 2),
          ShopItem(item = Orange, quantity = 3),
          ShopItem(item = Banana, quantity = 2)
        )
      ) shouldBe "Apples: £1.20\nOranges: £0.75\nBananas: £0.40\nDiscounts Applied: £0.65\nTotal to pay: £1.7"
    }

    "printAndCalculateBill should calculate the sum of a list using 2 for 1 for bananas when there are no apples" in {
      testCheckout.printAndCalculateBill(
        List(
          ShopItem(item = Orange, quantity = 3),
          ShopItem(item = Banana, quantity = 2)
         )
      ) shouldBe "Oranges: £0.75\nBananas: £0.40\nDiscounts Applied: £0.45\nTotal to pay: £0.7"
    }
  }
}

