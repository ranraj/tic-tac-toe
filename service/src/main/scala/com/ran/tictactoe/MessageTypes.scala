package com.ran.tictactoe

import spray.json.DefaultJsonProtocol

object MessageType {
  sealed trait Message
  case class GameMessage(sender: String,gameId:String, message: String) extends Message
  case class Joined(member: String,gameId:String , allMembers: Seq[String]) extends Message
  case class Left(member: String,gameId:String, allMembers: Seq[String]) extends Message
}
