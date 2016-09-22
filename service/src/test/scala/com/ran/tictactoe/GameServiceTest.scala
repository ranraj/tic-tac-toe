package com.ran.tictactoe

import akka.http.scaladsl.model.ws.{TextMessage, Message}
import akka.http.scaladsl.testkit.{WSProbe, ScalatestRouteTest}
import org.scalatest.{FlatSpecLike, FunSuite}
import upickle.default._

/**
 * Created by ranjithrajd on 22/9/16.
 */
class GameServiceTest extends FlatSpecLike with ScalatestRouteTest {
  "join api test " should " response by webSocket connection " in {
    val wsClient = WSProbe()

    val PLAYER_NAME = "PlayerA"
    val GAME_ID = "OX"
    val PING_MESSAGE = "Ping!"

    WS(Api.join(PLAYER_NAME, GAME_ID), wsClient.flow) ~>
      new GameService().route ~>
      check {
        /* Player joining event should return player details in Joined type  */
        val joinMsg: Message = wsClient.expectMessage()
        val joinedMessage = joinMsg match {
          case TextMessage.Strict(msg) => read[Joined](msg.toString)
        }

        assert(joinedMessage.member.name.equals(PLAYER_NAME))
        assert(joinedMessage.gameId.equals(GAME_ID))

        /* Player messaging event should return Message type with message */
        wsClient.sendMessage(PING_MESSAGE)

        val pingMsg: Message = wsClient.expectMessage()
        val gameMessage = pingMsg match {
          case TextMessage.Strict(msg) => read[GameMessage](msg.toString)
        }
        assert(gameMessage.sender.equals(PLAYER_NAME))
        assert(gameMessage.message.equals(PING_MESSAGE))

        /* Closing socket connection */

        wsClient.sendCompletion()
        wsClient.expectCompletion()
      }
  }
}

object Api {
  def join(name: String, id: String) = "/game/join?name=" + name + "&id=" + id
}
