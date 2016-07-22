package com.sparkydots.utils

import java.io.File
import java.nio.file.Files
import org.scalatest._


class ServiceDiscoveryTest extends FlatSpec with Matchers {

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

}

