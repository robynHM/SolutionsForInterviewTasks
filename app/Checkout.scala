

//One

class Checkout {

    def checkoutDoubleList(shoppingList: List[Double]): Double = shoppingList.sum

  //alternative for list of string

    def checkoutStringList(shoppingList: List[String]): Double = {
      val apples: Double = shoppingList.count(_.trim.toLowerCase == "apple") * 0.60
      val oranges: Double = shoppingList.count(_.trim.toLowerCase == "orange") * 0.25
      val bananas: Double = shoppingList.count(_.trim.toLowerCase == "banana") * 0.20
      apples + oranges + bananas
    }

  //Two

  private def buyOneGetOneFree(shopping: List[Double]): Double = {
    if (((shopping.length / 2.0) - (shopping.length / 2)) > 0) {
      shopping.drop(1).sum / 2 + shopping.head
    } else shopping.sum / 2
  }

  private def threeForTwo(shopping: List[Double]): Double = {
    val remainder = ((shopping.length / 3.0) - (shopping.length / 3))
    if (remainder > 0) {
      val orangesNotInOffer = (remainder / 0.33).toInt
      (shopping.drop(orangesNotInOffer).sum / 3) * 2 + (orangesNotInOffer * shopping.head)
    } else (shopping.sum / 3) * 2
  }

  private def appleAndBananasDiscount(bananas: List[Double], apples: List[Double]): Double = {
    val applesMinusBananas: Int = apples.length - bananas.length
    if (applesMinusBananas < 0) {
      apples.sum + ((applesMinusBananas * -1) * 0.20)
    } else if (applesMinusBananas == 0) {
      apples.sum
    } else {
      (bananas.length * 0.6) + (applesMinusBananas * 0.6)
    }
  }

  def checkoutDoublesTotalBill(shoppingList: List[Double]): Double = {
    val bananas: List[Double] = shoppingList.filter(_ == 0.20)
    val apples: List[Double] = shoppingList.filter(_ == 0.60)
    val oranges: List[Double] = shoppingList.filter(_ == 0.25)

    if(bananas.nonEmpty && apples.nonEmpty){
      appleAndBananasDiscount(bananas = bananas, apples = apples) + threeForTwo(oranges)
    } else {
      buyOneGetOneFree(bananas) + threeForTwo(oranges) + buyOneGetOneFree(apples)
    }


  }

  //alternative using strings based on exercise hint

    def checkoutStringsTotalBill(shoppingList: List[String]): Double = {
      val convertToDoubleList: List[Double] = shoppingList.map { item =>
        item.trim.toLowerCase match {
          case "apple" => 0.60
          case "orange" => 0.25
          case "banana" => 0.20
        }
      }
      checkoutDoublesTotalBill(convertToDoubleList)
    }






}
