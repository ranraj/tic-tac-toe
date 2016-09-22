package com.ran.tictactoe

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import scala.util.{ Success, Failure }
import scala.concurrent.{Await, Future}
import akka.http.scaladsl.server.Directives._

object Boot extends App {
  implicit val system = ActorSystem()
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  val config = system.settings.config
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")

  val serverSource: Source[Http.IncomingConnection, Future[Http.ServerBinding]] =
    Http().bind(interface = interface, port)

  /* Binding server list of routes */
  val binding: Future[Http.ServerBinding] =
    serverSource.to(Sink.foreach { connection =>
      println("Accepted new connection from {}", connection.remoteAddress)
      connection handleWith routes
    }).run()

  //TODO : Need to terminate server gracefully
  binding.onComplete {
    case Success(binding) ⇒
      val localAddress = binding.localAddress
      println(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")
    case Failure(e) ⇒
      println(s"Binding failed with ${e.getMessage}")
      system.shutdown()
  }

  val routes = {
    val serviceRoute = new GameService().route
    val healthCheckRoute = HealthCheck.route

    healthCheckRoute ~
    serviceRoute
  }
}
