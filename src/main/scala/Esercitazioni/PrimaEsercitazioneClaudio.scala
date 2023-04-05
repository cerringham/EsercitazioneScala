package Esercitazioni

object PrimaEsercitazioneClaudio extends App {

  def diffAssoluta(n: Int): Int = {
    val x = 51
    if (n > x) (n - x) * 3 else x - n
  }

  println(diffAssoluta(55))

  def removeCharAtIndex(str: String, index: Int): String = {
    if (index < 0 || index >= str.length) {
      return "Cannot find char at given index!"
    }
    val leftString = str.substring(0, index)
    val rightString = str.substring(index + 1)
    leftString + rightString
  }

  println(removeCharAtIndex("Hello, World", 4))

  def startsWithString(inputStr: String, startsString: String): Boolean = {
    inputStr.startsWith(startsString)
  }

  println(startsWithString("Hello, World", "He"))

  def oddFilter(startPoint: Int, endPoint: Int) {
    if (startPoint > endPoint) {
      return
    }
    if (startPoint % 2 != 0) {
      println(startPoint)
    }
    oddFilter(startPoint + 1, endPoint)
  }

  println(oddFilter(1, 5))

  def upperCaseString(input: String, lastChars: Int): String = {
    if (input.length < lastChars) {
      return input.toUpperCase
    }
    val upper = input.substring(input.length - lastChars, input.length).toUpperCase
    val remain = input.substring(0, input.length - lastChars)

    remain + upper
  }

  println(upperCaseString("Hello, World", 5))


  def calcolatrice(operazione: String, numA: Int, numB: Int) {
    operazione match {
      case "sum" => println("Somma " + (numA + numB))
      case "diff" => println("Differenta " + (numA - numB))
      case "mul" => println("Moltiplicazione " + (numA * numB))
      case "div" => if (numB == 0) throw new IllegalArgumentException("Cannot divide by 0") else println("Divisione " + (numA / numB))
      case "pow" => println("Power " + math.pow(numA, numB))
      case "fact" => println("Factoriale NumA " + factorial(numA) + "\nFactoriale NumB " + factorial(numB))
      case _ => throw new IllegalArgumentException("Invalid operation name")
    }
  }

  def factorial(n: Int): Int = {
    if (n <= 1) 1
    else n * factorial(n - 1)
  }

  calcolatrice("sum", 102, 98)

  calcolatrice("diff", 100, 50)

  calcolatrice("mul", 10, 8)

  calcolatrice("fact", 5, 4)

}
