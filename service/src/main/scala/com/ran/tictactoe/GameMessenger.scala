package com.ran.tictactoe

import akka.actor._
import akka.stream.OverflowStrategy
import akka.stream.scaladsl._
import MessageType._
trait GameMessenger {
  def gameFlow(sender: String, gameId: String): Flow[String, Message, Any]
  def pushMessage(message: GameMessage): Unit
}

object GameMessenger {
  def create(system: ActorSystem): GameMessenger = {
    // The implementation uses a single actor per chat to collect and distribute
    // chat messages. It would be nicer if this could be built by stream operations
    // directly.
    val chatActor =
      system.actorOf(Props(new Actor {

        var subMap = Map.empty[String, Set[(String, ActorRef)]]

        def receive: Receive = {
          case NewPlayer(name, gameId, subscriber) ⇒
            var subscribers = Set.empty[(String, ActorRef)]
            subscribers = subMap.get(gameId) match {
              case Some(sub) ⇒ {
                val a = sub ++ Set(name -> subscriber)
                subMap += (gameId -> a)
                a
              }
              case None ⇒ {
                val a = Set(name -> subscriber)
                subMap += (gameId -> a)
                a
              }
            }
            //println(subMap)
            dispatch(Joined(name, gameId, members(gameId)), gameId)

          case msg: ReceiveMessage      ⇒ dispatch(msg.toGameMessage, msg.gameId)
          case msg: GameMessage ⇒ dispatch(msg, msg.gameId)
          case PlayerLeft(person, gameId) ⇒
            var subscribers = Set.empty[(String, ActorRef)]
            subscribers = subMap.get(gameId) match {
              case Some(sub) ⇒ sub
              case None      ⇒ throw new Exception("Subscribers Set is not present for this game id")
            }
            val set @ (name, ref) = subscribers.find(_._1 == person).get
            // report downstream of completion, otherwise, there's a risk of leaking the
            // downstream when the TCP connection is only half-closed
            ref ! Status.Success(Unit)
            subscribers -= set
            subMap += (gameId -> subscribers)
            dispatch(Left(person, gameId, members(gameId)), gameId)
          case Terminated(sub) ⇒
            var subscribers = Set.empty[(String, ActorRef)]
            subMap = subMap.map(a ⇒ (a._1 -> a._2.filterNot(_._2 == sub)))
            // clean up dead subscribers, but should have been removed when `ParticipantLeft`
            subscribers = subscribers.filterNot(_._2 == sub)
        }
        //def sendAdminMessage(msg: String): Unit = dispatch(Protocol.ChatMessage("admin", msg))
        def dispatch(msg: Message, gameId: String): Unit = {
          var subscribers = Set.empty[(String, ActorRef)]
          subscribers = subMap.get(gameId) match {
            case Some(sub) ⇒ sub
            case None      ⇒ throw new Exception("Subscribers Set is not present for this game id")
          }
          subscribers.foreach(_._2 ! msg)
        }
        def members(gameId: String) = {
          var subscribers = Set.empty[(String, ActorRef)]
          subscribers = subMap.get(gameId) match {
            case Some(sub) ⇒ sub
            case None      ⇒ throw new Exception("Subscribers Set is not present for this game id")
          }
          subscribers.map(_._1).toSeq
        }
      }))

    // Wraps the chatActor in a sink. When the stream to this sink will be completed
    // it sends the `ParticipantLeft` message to the chatActor.
    // FIXME: here some rate-limiting should be applied to prevent single users flooding the chat
    def gameInSink(sender: String, gameId: String) = Sink.actorRef[GameEvent](chatActor, PlayerLeft(sender, gameId))

    new GameMessenger {
      def gameFlow(sender: String, gameId: String): Flow[String, GameMessage, Any] = {
        val in =
          Flow[String]
            .map(ReceiveMessage(sender, gameId, _))
            .to(gameInSink(sender, gameId))

        // The counter-part which is a source that will create a target ActorRef per
        // materialization where the chatActor will send its messages to.
        // This source will only buffer one element and will fail if the client doesn't read
        // messages fast enough.
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
