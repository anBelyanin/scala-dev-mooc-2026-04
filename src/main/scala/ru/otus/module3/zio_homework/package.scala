package ru.otus.module3

import zio.*

import java.io.IOException
import ru.otus.module3.zio_homework.config.*
import ru.otus.module3.zioConcurrency.printEffectRunningTime
import zio.config.*
import zio.config.magnolia.deriveConfig

import java.util.concurrent.TimeUnit
import scala.language.{existentials, postfixOps}

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */


  lazy val guessProgram: ZIO[Any, IOException, Unit] = for {
    digit <- Random.nextIntBetween(1, 4)
    _ <- Console.printLine("Я загадал число от 1 до 3. Какое это число?")
    userDigit <- Console.readLine
    _ <- if userDigit.toInt == digit then Console.printLine("Правильно!") else Console.printLine("Неправильно!")
  } yield ()


  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[A](effect: ZIO[Any, Exception, A], predicate: A => Boolean): ZIO[Any, Exception, A] = effect.flatMap { a =>
    if predicate(a) then ZIO.succeed(a) else doWhile(effect, predicate)
  }


  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   *    и выведет его в консоль
   *    Используйте эффект "Configuration.config" из пакета config
   */


  def loadConfigOrDefault: ZIO[Any, Exception, AppConfig] =
    ConfigProvider.envProvider.load(deriveConfig[AppConfig]).orElse {
      Configuration.config.zip {
        Configuration.config.flatMap {
          c => Console.printLine("Used default configuration: " + c.host + c.port)
        }
      }
    }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   *    обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   *    на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Any, Nothing, Int] =
    ZIO.sleep(1 seconds).flatMap { _ =>
      Random.nextIntBetween(0, 11)
    }


  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[ZIO[Any, Nothing, Int]] = {
    val list = List.newBuilder[ZIO[Any, Nothing, Int]]
    for i <- 1 to 10 do
      list += eff
    list.result()
  }


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Any, Nothing, Int] =
    printEffectRunningTime {
      effects.reduce { (eff1, eff2) =>
        eff1.flatMap { v1 =>
          eff2.map { v2 =>
            v1 + v2
          }
        }
      }.map { sum =>
        println("Result: " + sum)
        sum
      }
    }


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: ZIO[Any, Nothing, Int] =
    printEffectRunningTime {
      ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _).map { sum =>
        println("Result: " + sum)
        sum
      }
  }


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */
  object RunningTimePrinter {

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
      val currentTime: UIO[Long] = Clock.currentTime(TimeUnit.SECONDS)
      for {
        start <- currentTime
        r <- zio
        end <- currentTime
        _ <- ZIO.attempt(println(s"Running time: ${end - start}")).orDie
      } yield r
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */
  lazy val appWithTimeLogg: ZIO[Any, Nothing, Int] = RunningTimePrinter.printEffectRunningTime(app)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[Any, Nothing, Int] = appWithTimeLogg

}
