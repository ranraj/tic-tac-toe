package com.ran.tictactoe

import spray.json.DefaultJsonProtocol

import scala.concurrent.Future

/**
 * Created by ranjithrajd on 4/7/16.
 */

  case class GameCode(code: String)

  object GameCode {
    def apply() : GameCode = GameCode(randomString(5))
    def randomString(length: Int) = scala.util.Random.alphanumeric.take(length).mkString
  }
  trait MessageProtocols extends DefaultJsonProtocol {
    implicit val gameCodeFormat = jsonFormat1(GameCode.apply)
  }

  object GameCodeService{
    def fetchGameCode(): Future[GameCode] = {
      Future.successful(GameCode.apply())
    }
  }

