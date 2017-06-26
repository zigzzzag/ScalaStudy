package impatient

object chapter2 {
  def main(args: Array[String]): Unit = {
    countdown(10)
    unicodeMultiplyCycle("Hello")
    unicodeMultiply1("Hello")
    unicodeMultiply2("Hello")
    println(unicodeMultiplyRecursive("Hello"))

    println("pow(3, 3) = " + pow(3, 3))
    println("pow(2, 10) = " + pow(2, 10))
    println("pow(10, 2) = " + pow(10, 2))
    println("pow(10, -2) = " + pow(10, -2))
    println("pow(10, -1) = " + pow(10, -1))
  }

  def countdown(n: Int): Unit = {
    for (i <- (0 to n).reverse) print(i + " ")
    println()
  }

  def unicodeMultiplyCycle(str: String): Unit = {
    var res: Long = 1L
    for (c <- str) res *= c.toInt
    println(res)
  }

  def unicodeMultiply1(str: String): Unit = {
    val res = str.foldLeft(1L)((l: Long, c: Char) => l * c.toInt)
    println(res)
  }

  def unicodeMultiply2(str: String): Unit = {
    val res: Long = str.map(_.toLong).product
    println(res)
  }

  def unicodeMultiplyRecursive(str: String): Long = {
    if (str.isEmpty) {
      1
    } else {
      str.head.toLong * unicodeMultiplyRecursive(str.tail)
    }
  }

  def pow(x: Double, n: Int): Double = {
    if (n == 0) {
      1
    }
    else if (n > 0) {
      if (n % 2 == 0) {
        val y = pow(x, n / 2)
        y * y
      } else {
        x * pow(x, n - 1)
      }
    } else {
      1 / pow(x, -n)
    }
  }
}
