package Esercitazioni

object CollectionsExercises extends App {

  def sumOfEvenNumbers(numbers : List[Int]): Int = {
    numbers.filter(n=>n%2==0).sum
  }

  def wordsStartingWithLetter(words: List[String], letter: Char): List[String] = {
    words.filter(_.startsWith(letter.toString)).sorted
  }

  def findOldestPerson(people : List[(String, Int)]): String = {
    people.sortWith((a, b) => a._2 > b._2).head._1
  }

  def findLongestString(strings : List[String]) : String = {
    strings.sortWith((s1, s2) => s1.length > s2.length).head
  }


  val numbers : List[Int] = (1 to 10).toList
  println(sumOfEvenNumbers(numbers))

  val words : List[String] = List("Hello", "I", "am", "Tommaso", "Giovannelli",
    "and", "I", "am", "26", "years", "old")
  println(wordsStartingWithLetter(words, 'a'))

  val people: List[(String, Int)] = List(("Tom",26),("Alex",18),("Marc",33)
    ,("John",42),("Judy",15),("Anna",59))
  println(findOldestPerson(people))

  println(findLongestString(words))
}
