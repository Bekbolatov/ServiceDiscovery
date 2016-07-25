package com.sparkydots.service

import java.io.{File, StringWriter, PrintWriter}

import scala.concurrent.{Future, ExecutionContext, Promise}
import scala.util.Try

/**
  * Service registry/discovery backed by FS
  */
class ServiceDiscovery {

  val REGISTRY_LOC = "/EFS/run/services"
  val MAX_FRESH_TIME_MILLIS: Long = 300000


  private def callService[T](serviceName: String,
                     forceRefresh: Boolean = false,
                     log: Option[( => String) => Unit] = None)
                    (f: (ServiceInstance) => Future[Option[T]])
                    (implicit executionContext: ExecutionContext): Future[Option[T]] = {

    val promise = Promise[Option[T]]()

    val service = getService(serviceName, forceRefresh)
    val serviceCall = service.call(f, log)

    serviceCall.onSuccess {
      case Some(r) =>
        promise.success(Some(r))
        log.foreach(_(s"Received value using first (cached) service $service"))
      case None =>
        log.foreach(_(s"None received using first (cached) service $service"))
        if (forceRefresh) {
          log.foreach(_("Not refreshing service."))
          promise.success(None)
        } else {
          log.foreach(_("Will try refreshing service instance list now"))
          promise.completeWith(callService(serviceName, true, log = log)(f))
        }
    }

    serviceCall.onFailure { case thrown =>
      log.foreach(_(s"Failure during service call: $service"))
      val sw = new StringWriter
      thrown.printStackTrace(new PrintWriter(sw))
      log.foreach(_ (sw.toString))
      if (forceRefresh) {
        log.foreach(_("Not refreshing service."))
        promise.success(None)
      } else {
        log.foreach(_("Will try refreshing service instance list now"))
        promise.completeWith(callService(serviceName, true, log = log)(f))
      }
    }
    promise.future
  }

  /**
    * Call service - first try all cached instances in random order, then read fresh values and try again.
    * @param serviceName
    * @param log optionally provide logging
    * @param f if function returns Future(None) - it will indicate that it was a failure
    * @param executionContext
    * @tparam T
    * @return
    */
  def call[T](serviceName: String, log: Option[( => String) => Unit] = None)
             (f: (ServiceInstance) => Future[Option[T]])
             (implicit executionContext: ExecutionContext): Future[Option[T]] =
    callService(serviceName, log = log)(f)

  private case class CachedService(serviceName: String, lastUpdatedMillis: Long, expirationTimeMillis: Long, service: Service)

  private var cachedServices: Seq[CachedService] = Seq()

  def getService(serviceName: String, forceRefresh: Boolean = false): Service = {
    val curTimeMillis = System.currentTimeMillis
    val minExpirationTimeMillis = if (forceRefresh) Long.MaxValue else curTimeMillis

    cachedServices.
      find(cs => cs.serviceName == serviceName && cs.expirationTimeMillis > minExpirationTimeMillis).
      map(_.service) getOrElse updateCachedService(serviceName, curTimeMillis)
  }

  /**
    * Update and return a service - synchronized, potentially with 0 instances (but updated lastUpdatedMillis).
    *
    * @param serviceName
    * @param requestTime
    * @return
    */
  private def updateCachedService(serviceName: String, requestTime: Long): Service = synchronized {
    cachedServices.
      find(cs => cs.serviceName == serviceName && cs.lastUpdatedMillis > requestTime).
      map(_.service) getOrElse {
      val instances = Try {
        getListOfFiles(s"$REGISTRY_LOC/$serviceName/").flatMap { file =>
          val pieces = file.getName.split(":")
          if (pieces.size == 2) {
            Some(ServiceInstance(pieces(0), pieces(1).toInt))
          } else {
            None
          }
        }
      }.getOrElse(Seq())

      val service = Service(instances)
      val curTimeMillis = System.currentTimeMillis
      val cachedService = CachedService(serviceName, curTimeMillis, curTimeMillis + MAX_FRESH_TIME_MILLIS, service)
      cachedServices = Seq(cachedService) ++ cachedServices.filter(c => c.serviceName != serviceName)
      service
    }
  }

  private def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.
        filter(_.isFile).
        sortBy(_.lastModified()).
        toList
    } else {
      List[File]()
    }
  }

}
