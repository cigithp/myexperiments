package main.miscellaneous

/**
  * Created on 8/12/17.
  *
  * Given a value N. Find out sum all prime numbers till N.
  * Time complexity
  * Space complexity
  *
  *
  */
object SumAndProduct extends App {

  def sum(n: Int)(f : Int => Boolean) : Int = {
    def f1(acc: Int, y: Int): Int = y match {
      case 1 => acc
      case _ if f(y) => f1(acc + y, y-1)
      case _ => f1(acc, y-1)
    }
    f1(0, n-1)
  }

  def product(n: Int)(f : Int => Boolean) : Int = {
    def f1(acc: Int, y: Int): Int = y match {
      case 1 => acc
      case _ if f(y) => f1(acc * y, y-1)
      case _ => f1(acc, y-1)
    }
    f1(1, n-1)
  }

  def isPrime(x: Int) : Boolean = x match {
    case 1 => false
    case 2 => true
    case _ => !(2 to (x-1)).exists(a => x % a == 0)
  }

  def isEven(x: Int) : Boolean = x match {
    case 0 => false
    case _ if x % 2 == 0 => true
    case _ => false
  }

  def isOdd(x: Int) : Boolean = x match {
    case 0 => false
    case _ if x % 2 != 0 => true
    case _ => false
  }

  println(s"s Result of sum of all prime numbers till 20 : ${sum(20)(isPrime)}")
  println(s"s Result of sum of all even numbers till 20 : ${sum(20)(isEven)}")
  println(s"s Result of sum of all odd numbers till 20 : ${sum(20)(isOdd)}")

  println(s"s Result of sum of all prime numbers till 20 : ${product(20)(isPrime)}")
  println(s"s Result of sum of all even numbers till 20 : ${product(20)(isEven)}")
  println(s"s Result of sum of all odd numbers till 20 : ${product(20)(isOdd)}")

}
