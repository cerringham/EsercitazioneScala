package Esercitazioni

object PrimaEsercitazione extends App {

  def distanceFrom51(n: Int) = {
    if (n > 51) 3 * (n - 51)
  }

  def removeStringCharacter(string : String, index : Int) : String = {
    if (index < string.length) string.substring(0, index) + string.substring(index+1, string.length)
    else throw new Exception(s"Index out of range for string with a length of ${string.length}")
  }

  def checkStringStart(string : String) : Boolean = {
    string.substring(0,2) == "Sc"
  }

  def printIntegersBetween40And150 () : Unit = {
    (40 to 150).toList.filter(n => n%2 != 0).foreach(n=> println(n))
  }

  def convertLast4CharactersToUppercase( string : String) : String = {
    if (string.length >= 4) string.substring(0, string.length - 4) + string.substring(string.length - 4, string.length).toUpperCase()
    else throw new Exception(s"The given string is too short")
  }


  println(distanceFrom51(55))
  println(removeStringCharacter("Ciao",3))
  println(checkStringStart("Sciarpa"))
  println(printIntegersBetween40And150())
  println(convertLast4CharactersToUppercase("ciaoooo"))
}
