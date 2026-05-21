package ru.otus.module1



import ru.otus.module1.variance.{Animal, Cat, animalFeeder}

import java.util.Optional
import scala.language.postfixOps



/**
 * referential transparency
 */


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int =
    if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int): Int = {
    def loop(n: Int, accum: Int): Int =
      if(n <= 0) accum
      else loop(n - 1, n * accum)
    loop(n, 1)
  }



  /**
   * Реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */
  def fibonacciDigit(n: Int): Int = {
    def calculate(index: Int, past1: Int, past2: Int): Int = {
      val sum: Int = past1 + past2
      if (index == n) sum
      else calculate(index + 1, sum, past1)
    }
    if (n == 0) 0
    else if (n == 1) 1
    else calculate(2, 1, 0)
  }

}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a =>
    val start = System.currentTimeMillis()
    val result = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result



  // изменение поведения ф-ции

  def isOdd(i: Int): Boolean = i % 2 > 0
  lazy val isEven: Int => Boolean = not(isOdd)
  def not[A](f: A => Boolean): A => Boolean = a => !f(a)



  // изменение самой функции

  def sum(x: Int, y: Int): Int = x + y

  def curried[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  curried(sum) // Int => Int => Int

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = curried(f)(a)

  val r: Int => Int = partial2(2, sum)
  r(3) // 5

}


object variance {


  // Invariance Вне зависимости от отношений между типами A и B, Box[A] и Box[B] два разных типа
  // + Covariance Если А является подтипом В, то Box[A] является подтипом Box[B]
  // - Contravariance Если А является подтипом В, то Box[A] является супер типом Box[B]

  class Box[+T](val item: T)

  class Feeder[-T] {
    def feed(v: T): Unit = println("Feeding")
  }

  sealed trait Animal

  case class Cat() extends Animal

  case class Dog() extends Animal

  val animalFeeder: Feeder[Animal] = Feeder[Animal]()
  val catFeeder: Feeder[Cat] = catFeeder
  catFeeder.feed(Cat())

  def feed(a: Animal): Unit = ???

  feed(Cat())
  feed(Dog())

  // trait Function1[-R, +T] = R => T

  val f1 : Animal => Dog = ???
  val f2: Dog => Animal = f1




}






/**
 *  Реализуем тип Option
 */



 object opt {


  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутствие результата
   */


  sealed trait Option[+T] {
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] =
      if (isEmpty) None
      else f(this.asInstanceOf[Some[T]].v)

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = if (!isEmpty) println(this.asInstanceOf[Some[T]].v)

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[TT >: T, B](augend: Option[TT], zipFunc: (opt1: Option[T], opt2: Option[TT]) => Option[B]): Option[B] = {
      if (augend.isEmpty) zipFunc(this, None)
      else zipFunc(this, augend)
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(predicate: (opt: T) => Boolean): Option[T] = {
      if (!isEmpty && predicate(this.asInstanceOf[Some[T]].v)) this
      else None
    }
  }

  object Option {
    def apply[T](v: T): Option[T] = Some(v)
  }

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  var animalOpt: Option[Animal] = None
  var intOpt: Option[Int] = ???

 }

 object list {
   /**
    *
    * Реализовать одно связанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */

   sealed trait List[+T] {
     def ::[TT >: T](elem: TT): List[TT] = new::[TT](elem, this)

     /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
     def reverse[TT >: T](): List[TT] = {
       def doReverse(element: List[TT], accumulator: List[TT]): List[TT] = element match {
         case Nil => accumulator
         case ::(head: TT, tail: List[TT]) => doReverse(tail, head :: accumulator)
       }

       doReverse(this, Nil)
     }


     /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */
     def map[B](f: T => B): List[B] = {
       def doMap(element: List[T], accumulator: List[B]): List[B] = {
         if (element.isInstanceOf[Nil.type]) accumulator.reverse()
         else {
           val cons = element.asInstanceOf[::[T]]
           doMap(cons.tail, f(cons.head) :: accumulator)
         }
       }

       doMap(this, Nil)
     }

     /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */
     def filter[T](predicate: T => Boolean): List[T] = {
       def doFilter(head: T, tail: List[T], accumulator: List[T]): List[T] = {
         if (tail.isInstanceOf[Nil.type]) accumulator
         else {
           val tailCons = tail.asInstanceOf[::[T]]
           if (predicate(head)) {
             doFilter(tailCons.head, tailCons.tail, head :: accumulator)
           }
           else doFilter(tailCons.head, tailCons.tail, accumulator)
         }
       }

       this match {
         case thisCons: ::[T] =>
           doFilter(thisCons.head, thisCons.tail, Nil)
         case _ => Nil
       }
     }

   }


   case class ::[A](head: A, tail: List[A]) extends List[A]

   case object Nil extends List[Nothing]

   object List {
     def apply[A](v: A*): List[A] =
       if (v.isEmpty) Nil else ::(v.head, apply(v.tail: _*))
   }

   /**
    *
    * Написать функцию incList которая будет принимать список Int и возвращать список,
    * где каждый элемент будет увеличен на 1
    */
   val incList: List[Int] => List[Int] = _.map(element => element + 1)

   /**
    *
    * Написать функцию shoutString которая будет принимать список String и возвращать список,
    * где к каждому элементу будет добавлен префикс в виде '!'
    */
   val shoutString: List[String] => List[String] = _.map(element => "!".concat(element))

    /**
     * Конструктор, позволяющий создать список из N - го числа аргументов
     * Для этого можно воспользоваться *
     *
     * Например, вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
     * def printArgs(args: Int*) = args.foreach(println(_))
     */
 }