package ru.otus.module1

import scala.util.Random

class WhiteBallRandomGetter {

  private type BALL = Int

  private val whiteBall: BALL = 1
  private val blackBall: BALL = 0

  private val bucket: List[BALL] = List(whiteBall, blackBall, whiteBall, blackBall, whiteBall, blackBall)

  private val getTwoRandomBalls: List[BALL] =
    Random.shuffle(bucket).take(2)

  val isWhiteBallGot: Boolean =
    getTwoRandomBalls.contains(1)
}
