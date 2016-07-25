package com.sparkydots.service

import java.io.File
import java.nio.file.Files

import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.concurrent.ScalaFutures

class ServiceDiscoveryTest extends FlatSpec with Matchers with ScalaFutures {

  val resourcesDirectory = new File("src/test/resources").getAbsolutePath

  def log(msg: => String) = println(msg)

  def newServiceDiscovery = {
    new ServiceDiscovery() {
      override val REGISTRY_LOC = resourcesDirectory + "/EFS/run/services"
    }
  }

  "Service" should "use the one that works" in {
    val sd = new ServiceDiscovery()

    println(sd.getService("latex2pdf"))
  }

  it should "return empty service when none available" in {
    val sd = new ServiceDiscovery() {
      override val REGISTRY_LOC = "impossibleloc"
    }
    sd.getService("latex2pdf") shouldBe Service(Seq())
  }

  it should "find services" in {
    val dir = Files.createTempDirectory("blah")
    val serv = Files.createTempDirectory(dir, "serv")
    val serviceName = serv.getFileName.toFile.getName
    val serviceTouch = java.io.File.createTempFile("someservice", ":4567", serv.toFile)
    val hostname = serviceTouch.getName

    println(dir.toFile.getName)
    println(serviceName)
    println(serviceTouch.getAbsolutePath)

    val sd = new ServiceDiscovery() {
      override val REGISTRY_LOC = dir.toFile.getPath
    }
    sd.getService(serviceName) shouldBe Service(Seq(ServiceInstance(hostname.dropRight(5), 4567)))
  }

  it should "find service" in {
    val resourcesDirectory = new File("src/test/resources").getAbsolutePath
    val sd = new ServiceDiscovery() {
      override val REGISTRY_LOC = resourcesDirectory + "/EFS/run/services"
    }
    sd.getService("example") shouldBe
      Service(
        Seq(
          ServiceInstance("1.2.3.4", 9000),
          ServiceInstance("1.2.3.5", 9001)
        ))
  }

  it should "fail, re-read and fail again" in {
    val sd = newServiceDiscovery
    val result = sd.call[String]("example", log = Some(log)) { instance =>
      Future(None)
    }

    whenReady(result) { s =>
      s shouldBe None
    }
  }

  it should "find result in first try" in {
    val sd = newServiceDiscovery
    sd.call[String]("example", log = Some(log)) {
      case ServiceInstance(_, 9000) => Future(None)
      case ServiceInstance(_, 9001) => Future(Some("test"))
    }
  }

  it should "fail when throwable" in {
    val sd = newServiceDiscovery
    sd.call[String]("example", log = Some(log)) {
      _ => Future.failed[Option[String]](new Exception("TestException"))
    }
  }

}

