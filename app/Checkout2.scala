class Checkout2 {

  def printAndCalculateBill(shoppingList: List[ShopItem]): String = {

    val apples: ShopItem = ShopItem(Apple, shoppingList.filter(item => item.item == Apple).map(_.quantity).sum)
    val oranges: ShopItem = ShopItem(Orange, shoppingList.filter(item => item.item == Orange).map(_.quantity).sum)
    val bananas: ShopItem = ShopItem(Banana, shoppingList.filter(item => item.item == Banana).map(_.quantity).sum)

    val totalBeforeDiscount = (apples.quantity * apples.item.price) + (bananas.quantity * bananas.item.price) + (oranges.quantity * oranges.item.price)

    if (bananas.quantity != 0 && apples.quantity != 0) {
      val total = ShopItem.buyOneGetCheapestFree(apples, bananas) + ShopItem.threeForTwo(oranges)
      shoppingList.map(item => item.item.getClass.getName.dropRight(1) + "s" + ":" + " " + "£" +BigDecimal(item.item.price * item.quantity).setScale(2, BigDecimal.RoundingMode.HALF_UP).toString + "\n").mkString +
        s"Discounts Applied: £${BigDecimal(totalBeforeDiscount - total).setScale(2, BigDecimal.RoundingMode.HALF_DOWN)}" +
        s"\nTotal to pay: £$total"
    } else if (apples.quantity != 0) {
        val total = ShopItem.threeForTwo(oranges) + ShopItem.buyOneGetOneFree(apples)
      shoppingList.map(item => item.item.getClass.getName.dropRight(1) + "s" + ":" + " " + "£" +BigDecimal(item.item.price * item.quantity).setScale(2, BigDecimal.RoundingMode.HALF_UP).toString + "\n").mkString +
          s"Discounts Applied: £${BigDecimal(totalBeforeDiscount - total).setScale(2, BigDecimal.RoundingMode.HALF_DOWN)}" +
          s"\nTotal to pay: £$total"

    } else {
        val total = ShopItem.buyOneGetOneFree(bananas) + ShopItem.threeForTwo(oranges)
      shoppingList.map(item => item.item.getClass.getName.dropRight(1) + "s" + ":" + " " + "£" +BigDecimal(item.item.price * item.quantity).setScale(2, BigDecimal.RoundingMode.HALF_UP).toString + "\n").mkString +
          s"Discounts Applied: £${BigDecimal(totalBeforeDiscount - total).setScale(2, BigDecimal.RoundingMode.HALF_DOWN)}" +
          s"\nTotal to pay: £$total"
    }

  }

}
