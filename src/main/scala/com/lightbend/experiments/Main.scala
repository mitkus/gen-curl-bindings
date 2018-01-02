package com.lightbend.experiments

import scala.io.Source

object Main extends Greeting with App {
  val src = Source.fromResource("curl.h")

  val nodes = Generator.nodes(src.mkString)
  val enums = Generator.getEnums(nodes)

  println(Generator.enumBinding("CURLINFO", enums).mkString("\n"))
  println(Generator.enumBinding("CURLoption", enums).mkString("\n"))
  println(Generator.enumBinding("CURLcode", enums).mkString("\n"))

}

trait Greeting {
  lazy val greeting: String = "hello"
}
