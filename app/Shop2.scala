sealed abstract class Item(val price: Double)
case class ShopItem(item: Item, quantity: Int)

object ShopItem {

  def buyOneGetOneFree(shopItem: ShopItem): Double = {
    if (((shopItem.quantity / 2.0) - (shopItem.quantity / 2)) > 0) {
      ((shopItem.quantity - 1) * shopItem.item.price / 2) + shopItem.item.price
    } else shopItem.quantity * shopItem.item.price / 2
  }

  def threeForTwo(shopItem: ShopItem): Double = {
    val remainder = ((shopItem.quantity / 3.0) - (shopItem.quantity / 3))
    if (remainder > 0) {
      val orangesNotInOffer = (remainder / 0.33).toInt
      ((shopItem.quantity - orangesNotInOffer) * shopItem.item.price / 3) * 2 + (orangesNotInOffer * shopItem.item.price)
    } else ((shopItem.quantity * shopItem.item.price) / 3) * 2
  }

  def buyOneGetCheapestFree(item1: ShopItem, item2: ShopItem): Double = {
    val cheapest = if(item1.item.price < item2.item.price) item1 else item2
    val mostExpensive = if(item1.item.price > item2.item.price) item1 else item2
    val minusCheapest: Int = mostExpensive.quantity - cheapest.quantity
    if (minusCheapest < 0) {
      (mostExpensive.quantity * mostExpensive.item.price) + ((minusCheapest * -1) * cheapest.item.price)
    } else if (minusCheapest == 0) {
      mostExpensive.quantity * mostExpensive.item.price
    } else {
      (cheapest.quantity * mostExpensive.item.price) + (minusCheapest * mostExpensive.item.price)
    }
  }
}

case object Apple extends Item(0.6)

case object Orange extends Item(0.25)

case object Banana extends Item(0.2)

