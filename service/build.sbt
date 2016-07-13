name := "TicTacToe-Service"
organization := "com.ran"
version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= {
  val akkaVersion       = "2.4.3"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-stream" % akkaVersion,
    "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion,
    "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaVersion,
    "com.lihaoyi" %% "upickle" % "0.2.8"
  )
}