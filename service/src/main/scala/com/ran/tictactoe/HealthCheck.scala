package com.ran.tictactoe

import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.scaladsl.Flow
/**
 * Created by ranjithrajd on 22/9/16.
 */
object HealthCheck {
  val healthCheckService: Flow[Message, Message, Any] = Flow[Message].map {
    case TextMessage.Strict(txt) => TextMessage("Echo " + txt)
    case _ => throw new Exception("We don't support other than Strict message")
  }

  def route: Route =
    pathPrefix("game") {
      get {
      path("healthCheck") {
          handleWebSocketMessages(healthCheckService)
        }
      }
  }
}
