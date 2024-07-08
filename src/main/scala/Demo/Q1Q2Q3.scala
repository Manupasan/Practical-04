@main def main() =
  val itemname = Array("apple", "mango", "banana")
  val quantity = Array(3, 4, 5)
  displayInventory(itemname, quantity)

  println("Enter item name to add : ")
  var x = scala.io.StdIn.readLine()
  println("Enter item quantity to add : ")
  var y = scala.io.StdIn.readInt()
  restockItem(itemname, quantity, x, y)
  displayInventory(itemname, quantity)

  println("Enter item name to sell : ")
  var p = scala.io.StdIn.readLine()
  println("Enter item quantity to sell: ")
  var q = scala.io.StdIn.readInt()
  sellItem(itemname, quantity, p, q)
  displayInventory(itemname, quantity)

  println("Enter a number : ")
  var number = scala.io.StdIn.readInt()
  PatternMatching(number)
  println()

  println("Uppercase - " + toUpper("AbCdE"))
  println("Lowercase - " + toLower("aBcDe"))
  println("To uppercase - " + formatNames("abcde")(toUpper))
  println("To lowercase - " + formatNames("ABCDE")(toLower))

def  displayInventory(arr1 : Array[String], arr2 : Array[Int]) : Unit =
  println("Remaining items are: ")
    for(i <- 0 to (arr1.length - 1))
      print(arr1(i) + "    -     " + arr2(i) + "\n")

def restockItem(arr1 : Array[String], arr2 : Array[Int], item :String, quant : Int) : Unit =
  if(arr1.contains(item))
    arr2(arr1.indexOf(item)) = arr2(arr1.indexOf(item)) + quant
  else
    println("Item does not exist!\n")

def sellItem(arr1 : Array[String], arr2 : Array[Int], item :String, quant : Int) : Unit =
  if(arr1.contains(item))
    if(arr2(arr1.indexOf(item)) >= quant)
      arr2(arr1.indexOf(item)) = arr2(arr1.indexOf(item)) - quant
    else
      println("There is not enough quantity to sell\n")
  else
    println("Item does not exist!\n")

def PatternMatching(num : Int) =
  num match
    case num if(num <=0) => println(num + " is Negative/Zero")
    case num if(num%2 == 0) => println(num + " is an Even number")
    case _ => println(num + " is an Odd number")

def toUpper(word: String): String =
  val arr0: Array[Char] = word.toCharArray
  var arr1 = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  var arr2 = Array('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')

  for (i <- 0 to (word.length - 1))
    for (j <- 0 to 51)
      if (arr0(i) == arr1(j))
        arr0(i) = arr2(j)

  arr0.mkString("")

def toLower(word : String) : String =
  val arr0 : Array[Char] = word.toCharArray
  var arr1 = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
  var arr2 = Array('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')

  for(i <- 0 to (word.length - 1))
    for(j <- 0 to 25)
      if(arr0(i) == arr2(j))
        arr0(i) = arr1(j)

  arr0.mkString("")

def formatNames(word : String)(func : (String) => String ) =
  func(word)
