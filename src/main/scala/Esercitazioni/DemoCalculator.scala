package Esercitazioni

object DemoCalculator extends App {

  def calculate(operation: String, numA: String, numB: String) = {
    try {
      val a: Int = numA.toInt
      val b: Int = numB.toInt

      operation.toLowerCase() match {
        case "sum" => a + b
        case "diff" => a - b
        case "mul" => a * b
        case "div" => a / b
        case "pow" => Math.pow(a, b)
        case "fact" => factorial(a)
        case _ => throw new Exception("Non-supported operation")
      }
    }
    catch {
      case e : NumberFormatException => println("Number string not valid")
      case e : Throwable => println(e.getMessage)
    }



  }

  private def factorial(n: Int): Int = {
    if (n == 1) n
    else n * factorial(n - 1)
  }

  println (factorial(4))
  println (calculate("div", "5", "0"))
}
