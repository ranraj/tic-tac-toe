package com.ran.tictactoe

import akka.http.scaladsl.testkit.{WSProbe, ScalatestRouteTest}
import org.scalatest.{FlatSpecLike, Matchers, FunSuite}

/**
 * WebSocket Api Health check
 */
class HealthCheckTest extends FlatSpecLike with ScalatestRouteTest {

  "healthCheck api" should "response by webSocket connection" in {
    val wsClient = WSProbe()

    WS("/game/healthCheck", wsClient.flow) ~> HealthCheck.route ~> check {

      wsClient.sendMessage("Ping!")
      wsClient.expectMessage("Echo Ping!")

      wsClient.sendCompletion()
      wsClient.expectCompletion()
    }
  }
}