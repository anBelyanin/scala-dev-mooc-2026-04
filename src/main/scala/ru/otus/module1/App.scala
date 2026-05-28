package ru.otus.module1

import ru.otus.module1.concurrency.{MyThread, getRatesLocation1, getRatesLocation2, printRunningTime}

import scala.util.{Failure, Success}


object App {
  def main(args: Array[String]): Unit = {
    println(s"Hello world from: " +
      s"${Thread.currentThread().getName}")

    var whiteBallRandomGetters: List[WhiteBallRandomGetter] = List()
    (1 to 1000000).foreach {_ =>
      whiteBallRandomGetters = new WhiteBallRandomGetter :: whiteBallRandomGetters
    }

    val whiteBallGettingResult: Double = whiteBallRandomGetters.map(getter => getter.isWhiteBallGot).count(result => result)

    println("Pairs with white balls ".concat(whiteBallGettingResult.toString))
    println("Collection size ".concat(whiteBallRandomGetters.size.toString))
    println("Theory result ".concat((whiteBallGettingResult / whiteBallRandomGetters.size).toString))

  }
}
