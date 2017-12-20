package com.lightbend.experiments

import scala.io.Source

object Main extends Greeting with App {
  val src = Source.fromResource("curl.h")
  val str = src.mkString
  Generator.generate(str)
}

trait Greeting {
  lazy val greeting: String = "hello"
}
