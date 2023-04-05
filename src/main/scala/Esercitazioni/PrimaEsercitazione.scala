package Esercitazioni

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object PrimaEsercitazione extends App {
  // Write a Scala program to get the absolute difference between n and 51. If n is greater than 51 return triple the absolute difference.
  def absDifference(n: Int): Int =
    if (n > 51) Math.abs(n - 51) * 3
    else Math.abs(n - 51)

  println(absDifference(23))
  println(absDifference(60))
  println(absDifference(-5))

  // Write a Scala program to remove the character in a given position of a given string
  // The given position will be in the range 0...string length -1 inclusive
  def removeChar(from: String, position: Int): Try[String] = {
    if (position >= from.length) Failure(new RuntimeException("position is greater than the string's length"))
    else Success(from.substring(0, position) + from.substring(position + 1))
  }
  removeChar("test", 1).foreach(println)

  // Write a Scala program to check whether a given string starts with 'Sc' or not.
  println("Scala".startsWith("Sc"))

  // Write a Scala program that print odd numbers in the range 40..150 inclusive
  println((40 to 150).filter(_ % 2 != 0))

  // Write a Scala program to convert the last 4 characters of a given string in upper case.
  // If the length of the string has less than 4 then uppercase all the characters.
  def uppercase4Chars(string: String): String = {
    val idx = Math.max(string.length - 4, 0)
    string.substring(0, idx) + string.substring(idx).toUpperCase
  }
  println(uppercase4Chars("Longer_Than_4chars"))
  println(uppercase4Chars("chr"))

  // Write a Scala program to simulate a calculator. The program take in input three String: operation, numA and numB
  // Valid operations are: sum, diff, mul, div, pow, fact(numA)
  // All other operations name are invalid
  @tailrec
  def fact(of: Int, acc: Int): Int = {
    if (of <= 0) acc
    else fact(of - 1, acc * of)
  }

  def calc(op: String, a: Int, b: Int): Try[Int] = op match {
    case "sum" => Success(a + b)
    case "diff" => Success(a - b)
    case "mul" => Success(a * b)
    case "div" if b != 0 => Success(a / b)
    case "div" => Failure(new RuntimeException("b cannot be zero"))
    case "fact" => Success(fact(a, 1))
    case _ => Failure(new RuntimeException("Invalid operation"))
  }
  def calc(op: String, numA: String, numB: String): Try[Int] = {
    val calculation = for {
      a <- numA.toIntOption
      b <- numB.toIntOption if (op != "fact")
      b <- numB.toIntOption
    } yield calc(op, a, b)
    calculation.getOrElse(Failure(new RuntimeException("Can't cast operators")))
  }
  println(calc("sum", "1", "2"))
  println(calc("fact", "5", "2"))
}
