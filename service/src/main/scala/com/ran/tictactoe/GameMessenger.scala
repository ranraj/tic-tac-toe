package com.ran.tictactoe

import akka.actor._
import akka.stream.OverflowStrategy
import akka.stream.scaladsl._
import MessageType._
sealed trait Player
final case object PlayerX extends  Player
final case object PlayerO extends Player
final case object NoPlayer extends Player

case class PlayerDetails(name:String,playerSymbol : Player)
trait GameMessenger {
  def gameFlow(sender: String, gameId: String): Flow[String, Message, Any]
  def pushMessage(message: GameMessage): Unit
}

object GameMessenger {
  def create(system: ActorSystem): GameMessenger = {
    val chatActor =
      system.actorOf(Props(new Actor {
        var subMap = Map.empty[String, Set[(PlayerDetails, ActorRef)]]

        def receive: Receive = {
          case NewPlayer(name, gameId, subscriber) ⇒
            subMap.get(gameId) match {
              case Some(sub) ⇒ {
                if (sub.size <= 1){
                  val newPlayerSymbol = if(!sub.isEmpty)
                        if(sub.head._1.playerSymbol == PlayerO) PlayerX else PlayerO
                        else PlayerO
                  val playerDetails = PlayerDetails(name, newPlayerSymbol)
                  val nameToSubscriber = sub ++ Set(playerDetails -> subscriber)
                  subMap += (gameId -> nameToSubscriber)
                  dispatch(Joined(playerDetails, gameId, members(gameId)), gameId)
                }
                else
                {
                    errorDispatch(ErrorMessage(name, gameId, "Room has been already Occupied"), subscriber)
                }
              }
              case None ⇒ {
                val playerDetails = PlayerDetails(name,PlayerO)
                val nameToSubscriber = Set(playerDetails -> subscriber)
                subMap += (gameId -> nameToSubscriber)
                dispatch(Joined(playerDetails, gameId, members(gameId)), gameId)
              }
            }
          case msg: ReceiveMessage      ⇒ dispatch(msg.toGameMessage, msg.gameId)
          case msg: GameMessage ⇒ dispatch(msg, msg.gameId)
          case PlayerLeft(person, gameId) ⇒
            var subscribers = getSubscribers(gameId)
            val set @ (playerDetails, ref) = subscribers.find(_._1.name == person).get
            ref ! Status.Success(Unit)
            subscribers -= set
            subMap += (gameId -> subscribers)
            dispatch(Left(playerDetails, gameId, members(gameId)), gameId)
          case Terminated(sub) ⇒
            var subscribers = Set.empty[(String, ActorRef)]
            subMap = subMap.map(a ⇒ (a._1 -> a._2.filterNot(_._2 == sub)))
            // clean up dead subscribers, but should have been removed when `ParticipantLeft`
            subscribers = subscribers.filterNot(_._2 == sub)
        }
        def dispatch(msg: Message, gameId: String): Unit = {
          getSubscribers(gameId).foreach(_._2 ! msg)
        }
        def errorDispatch(msg: Message, subscriber: ActorRef): Unit = {
          subscriber ! msg
        }
        def members(gameId: String) = {
          getSubscribers(gameId).map(_._1.name).toSeq
        }
        private def getSubscribers(gameId:String):Set[(PlayerDetails, ActorRef)] = {
          subMap.get(gameId) match {
            case Some(sub) ⇒ sub
            case None      ⇒ Set.empty[(PlayerDetails, ActorRef)] //throw new Exception("Subscribers Set is not present for this game id")
          }
        }
      }))

    def gameInSink(sender: String, gameId: String) = Sink.actorRef[GameEvent](chatActor, PlayerLeft(sender, gameId))

    new GameMessenger {
      def gameFlow(sender: String, gameId: String): Flow[String, GameMessage, Any] = {
        val in =
          Flow[String]
            .map(ReceiveMessage(sender, gameId, _))
            .to(gameInSink(sender, gameId))

        val out =
          Source.actorRef[GameMessage](1, OverflowStrategy.fail)
            .mapMaterializedValue(chatActor ! NewPlayer(sender, gameId, _))

        Flow.fromSinkAndSource(in, out)
      }
      def pushMessage(message: GameMessage): Unit = chatActor ! message // non-streams interface
    }
  }

  private sealed trait GameEvent
  private case class NewPlayer(name: String, gameId: String, subscriber: ActorRef) extends GameEvent
  private case class PlayerLeft(name: String, gameId: String) extends GameEvent
  private case class ReceiveMessage(sender: String, gameId: String, message: String) extends GameEvent {
    def toGameMessage: GameMessage = GameMessage(sender, gameId, message)
  }
}
