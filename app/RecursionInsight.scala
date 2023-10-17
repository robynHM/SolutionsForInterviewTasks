import java.util.Base64
import scala.annotation.tailrec

object RecursionInsight extends App{

  val stringList: List[String] = List("string 1",
    "string 2",
    "string 3"
  )

  //recursion
  //all the calls are kept in the call stack which has limited memory, adding a lot to the stack can
  // result in a stackoverflow error.
  def encodeList(notEncodedStrings: List[String], num: Int): String =
    if (notEncodedStrings.isEmpty) {
      ""
    } else {
      val firstString = notEncodedStrings.head
      val encoded = new String(Base64.getEncoder.encodeToString(firstString.getBytes()))
      val rules = s"service-config.rules.$num: " + encoded + "\n" + s"# $firstString" + "\n"
      rules + encodeList(notEncodedStrings.tail, num + 1)
    }
//call stack
//   service-config.rules.0: c3RyaW5nIDE= # string 1 + encodeList(List("string 1", "string 2", "string 3"), 0)
//   service-config.rules.1: c3RyaW5nIDI= # string 2 + encodeList(List("string 2", "string 3"), 1)
//   service-config.rules.2: c3RyaW5nIDM= # string 3 + encodeList(List("string 3"), 2)
//   encodeList(List()) empty so returns ""

  println("RECURSION OUTPUT:")
  println(encodeList(stringList, 0))

  //tail recursion
  //calling itself is the last action, this way the stack can be reused, as there is no data on the call
  //stack that needs to be remembered so rather than keep adding call to the stack we can use it like a
  // loop and reuse the stack.
  //No other operation or data needs to be saved when the function returns.
  //ensure something is using tail recursion by adding the @tailrec annotation,
  // if it is not tail recursive you will get an compile-time error.
  def methodTakesList(listStrings: List[String]): String = {
    @tailrec
    def tailEncodeList(notEncodedStrings: List[String], counter: Int, encodedString: String): String = {
      notEncodedStrings match {
        case Nil => encodedString
        case ::(head, tail) =>
          val encoded = new String(Base64.getEncoder.encodeToString(head.getBytes()))
          val encodedStrings: String = encodedString + s"service-config.rules.$counter: " + encoded + "\n" + s"# $head \n"
          tailEncodeList(tail, counter + 1, encodedStrings)
      }
    }
    tailEncodeList(listStrings, 0, "")
  }

  //call stack
  //1st call
  //tailEncodeList(
  // List("string 1", "string 2", "string 3"),
  // 0,
  // ""
  // )
  //2nd call
  //tailEncodeList(
  // List("string 2", "string 3"),
  // 1,
  // "service-config.rules.0: c3RyaW5nIDE= # string 1"
  // )
  //3rd call
  //tailEncodeList(
  // List("string 3"),
  // 2,
  // "service-config.rules.0: c3RyaW5nIDE= # string 1 service-config.rules.1: c3RyaW5nIDI= # string 2"
  // )
  //4th call
  //tailEncodeList(List()) list empty so returns the value of the encodedString =>
  // "service-config.rules.0: c3RyaW5nIDE= # string 1 service-config.rules.1: c3RyaW5nIDI= # string 2 service-config.rules.2: c3RyaW5nIDM= # string 3"

  println("TAIL OUTPUT:")
  println(methodTakesList(stringList))

  //zip with index
  def zipEncodeList(notEncodedStrings: List[String]): String =
    notEncodedStrings.zipWithIndex.map{
      case (listItem, index) =>
        val encoded = new String(Base64.getEncoder.encodeToString(listItem.getBytes()))
        s"service-config.rules.$index: " + encoded + "\n" + s"# $listItem" + "\n"
    }.mkString

  println("ZIP OUTPUT:")
  println(zipEncodeList(stringList))

  def whileEncoded(notEncodedStrings: List[String]): Unit = {
    var newList: List[String] = notEncodedStrings
    var counter = 0
    while (counter != notEncodedStrings.length) {
      val encoded = new String(Base64.getEncoder.encodeToString(notEncodedStrings.head.getBytes()))
      println(s"service-config.rules.$counter: " + encoded + "\n" + s"# ${newList.head}")
      counter = counter + 1
      newList = notEncodedStrings.tail
    }
  }

  println("WHILE:")
  println(whileEncoded(stringList))

  //foldLeft
  def foldLeftEncode(notEncodedStrings: List[String]) = notEncodedStrings.zipWithIndex.foldLeft(""){
    case (result: String, (value: String, index: Int)) =>
      val encoded = new String(Base64.getEncoder.encodeToString(value.getBytes()))
      result + s"service-config.rules.$index: " + encoded + "\n" + s"# $value" + "\n"
  }

  println("FOLD LEFT:")
  println(foldLeftEncode(stringList))

  //foldRight
  //will give you output backwards so the last processed is the 1st returned
  //string 3 is last in list but output will be first returned
  def foldRightEncode(notEncodedStrings: List[String]) = notEncodedStrings.zipWithIndex.foldRight("") {
    case ((value: String, index: Int), result: String) =>
      val encoded = new String(Base64.getEncoder.encodeToString(value.getBytes()))
      result + s"service-config.rules.$index: " + encoded + "\n" + s"# $value" + "\n"
  }

  println("FOLD RIGHT:")
  println(foldRightEncode(stringList))
}
