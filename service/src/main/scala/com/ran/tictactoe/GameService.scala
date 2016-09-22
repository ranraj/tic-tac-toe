package com.ran.tictactoe

import java.util.Date

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.stream.stage._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import scala.concurrent.duration._
import akka.http.scaladsl.server._
import akka.stream.Materializer
import akka.stream.scaladsl.Flow

import upickle.default._

/**
 * Created by ranjithrajd on 4/7/16.
 */

class GameService(implicit fm: Materializer, system: ActorSystem) extends Directives with MessageProtocols {
  val game = GameMessenger.create(system)
  val logger = Logging(system, getClass)

  import system.dispatcher

  system.scheduler.schedule(15.second, 15.second) {
    game.pushMessage(GameMessage(sender = "clock", gameId = "default", s"Bling! The time is ${new Date().toString}."))
  }

  def route: Route =
    logRequestResult("tictactoe-service") {
      respondWithHeader(RawHeader("Access-Control-Allow-Origin", "*")) {
        pathPrefix("game") {
          get {
            pathSingleSlash {
              getFromResource("web/index.html")
            } ~
              path("join") {
                parameters('name, 'id) { (name, id) ⇒
                  Directives.handleWebSocketMessages(webSocketChatFlow(sender = name, id = id))
                }
              } ~
              path("code" / "request") {
                parameters('name) { name ⇒
                  complete(GameCodeService.fetchGameCode)
                }
              }
          }
        }
      } ~
        getFromResourceDirectory("web")
    }

  def webSocketChatFlow(sender: String, id: String): Flow[Message, Message, Any] =
    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) ⇒ msg
      }
      .via(game.gameFlow(sender, id))
      .map {
        case msg: AppMessage ⇒
          TextMessage.Strict(write(msg))
      }
      .via(reportErrorsFlow)

  def reportErrorsFlow[T]: Flow[T, T, Any] =
    Flow[T]
      .transform(() ⇒ new PushStage[T, T] {
        def onPush(elem: T, ctx: Context[T]): SyncDirective = ctx.push(elem)

        override def onUpstreamFailure(cause: Throwable, ctx: Context[T]): TerminationDirective = {
          println(s"WS stream failed with $cause")
          super.onUpstreamFailure(cause, ctx)
        }
      })
}
