package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.task

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличие от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правовой результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =

    def process(currentFutures: List[Future[A]],
                successful: List[A],
                failed: List[Throwable]): Future[(List[A], List[Throwable])] = {
      currentFutures match {
        case head :: next => head.transformWith {
          case Success(value) => process(next, value :: successful, failed)
          case Failure(throwable) => process(next, successful, throwable :: failed)
        }
        case Nil => Future.apply((successful.reverse, failed.reverse))
      }
    }
    
    process(futures, List.empty[A], List.empty[Throwable])
    
}
