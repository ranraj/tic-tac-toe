package com.ran.tictactoe

import spray.json.DefaultJsonProtocol

object MessageType {
  sealed trait Message
  case class GameMessage(sender: String,gameId:String, message: String) extends Message
  case class Joined(member: PlayerDetails,gameId:String , allMembers: Seq[String] ) extends Message
  case class Left(member: PlayerDetails,gameId:String, allMembers: Seq[String]) extends Message
  case class ErrorMessage(sender: String,gameId:String, message: String) extends Message
}
