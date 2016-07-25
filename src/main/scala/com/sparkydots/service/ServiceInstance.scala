package com.sparkydots.service

case class ServiceInstance(host: String, port: Int) {

  lazy val httpUrl: String = s"http://$host:$port"
  lazy val httpsUrl: String = s"https://$host:$port"

  def httpUrl(path: String): String = httpsUrl + path

  def httpsUrl(path: String): String = httpsUrl + path

}
