package Esercitazioni

import scala.Console.println
import scala.util.Try

object PrimaEsercitazione extends App {

  // Write a Scala program to get the absolute difference between n and 51. If n is greater than 51 return triple
  // the absolute difference
  def testDifferenceFrom51(x: Int): Int = {
    val abs_Diff = Math.abs(x - 51)
    if (x > 51) 3 * abs_Diff else abs_Diff
  }

  println("Result: " + testDifferenceFrom51(60));
  println("Result: " + testDifferenceFrom51(40));

  // Write a Scala program to remove the character in a given position of a given string
  // The given position will be in the range 0...string length -1 inclusive
  def removeCharFromString(str: String, n: Int): String = {
    if (n > str.length)
      str
    str.take(n) + str.drop(n + 1)
  }

  println("Result: " + removeCharFromString("Scala", 1))
  println("Result: " + removeCharFromString("Scala", 0))
  println("Result: " + removeCharFromString("Scala", 4))
  println("Result: " + removeCharFromString("Scala", 43))

  // Write a Scala program to check whether a given string starts with 'Sc' or not
  def checkIfStringStartsWithSc(str1: String): Boolean = {
    str1.startsWith("Sc")
  }

  // Write a Scala program that print odd numbers in the range 40..150 inclusive
  def printOddNumbersInRange(lowerLimit: Int, upperLimit: Int) = {
    val aRange: Seq[Int] = lowerLimit until upperLimit
    val odd_nums = aRange.filter(_ % 2 != 0)
    odd_nums.foreach(x => println(x))
  }

  printOddNumbersInRange(40, 150)

  // Write a Scala program to convert the last 4 characters of a given string in upper case.
  // If the length of the string has less than 4 then uppercase all the characters.
  def convertLastForuCharToUpperCase(str1: String): String = {
    str1.take(str1.length() - 4) + str1.drop(str1.length() - 4).toUpperCase()
  }

  // Write a Scala program to simulate a calculator. The program take in input three String: operation, numA and numB
  // Valid operations are: sum, diff, mul, div, pow, fact(numA)
  // All other operations name are invalid
  def calculator(operation: String, firstNumber: String, secondNumber: String) = {
    operation match {
      case "sum" => Try {
        firstNumber.toInt + secondNumber.toInt
      }
      case "diff" => Try {
        firstNumber.toInt - secondNumber.toInt
      }
      case "mul" => Try {
        firstNumber.toInt * secondNumber.toInt
      }
      case "div" => Try {
        firstNumber.toInt / secondNumber.toInt
      }
      case "pow" => Try {
        math.pow(firstNumber.toInt, secondNumber.toInt)
      }
      case "fact" => Try {factorial(firstNumber.toInt)}
      case _ => "not allowed"
    }
  }

  def factorial(n: Int): Int =
    if (n <= 1) 1
    else {
      val result = n * factorial(n - 1)

      result
    }

  println(calculator("div", "cinque", "due"))
}
