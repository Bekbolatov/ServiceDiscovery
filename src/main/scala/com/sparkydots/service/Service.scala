package com.sparkydots.service

import scala.util.Random
import scala.concurrent.{Future, ExecutionContext, Promise}

case class Service(instances: Seq[ServiceInstance]) {

  def call[T](f: (ServiceInstance) => Future[Option[T]], log: Option[(=> String) => Unit] = None)
                              (implicit executionContext: ExecutionContext): Future[Option[T]] =
    callInstances[T](Random.shuffle(instances), f, log)

  private def callInstances[T](orderedInstances: Seq[ServiceInstance],
                               f: (ServiceInstance) => Future[Option[T]],
                               log: Option[(=> String) => Unit] = None)
                              (implicit executionContext: ExecutionContext): Future[Option[T]] = {
    val promise = Promise[Option[T]]()

    orderedInstances match {
      case Seq() =>
        promise.success(None)
      case firstInstance +: otherInstances =>
        val evalResult = f(firstInstance)
        evalResult.onSuccess {
          case Some(r) =>
            promise.success(Some(r))
            log.foreach(_ (s"Received value using instance $firstInstance"))
          case None =>
            promise.completeWith(callInstances(otherInstances, f, log))
            log.foreach(_ (s"None received using instance $firstInstance"))
        }
        evalResult.onFailure { case thrown =>
          log.foreach(_ (s"Error getting data from $firstInstance: "))
          thrown.printStackTrace()
          promise.completeWith(callInstances(otherInstances, f, log))
        }
    }

    promise.future
  }

}
