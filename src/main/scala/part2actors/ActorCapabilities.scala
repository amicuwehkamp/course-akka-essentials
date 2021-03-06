package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi!" => context.sender() ! "Hello back!"  // replying to a message
      case message: String => println(s"[simple actor] I have received $message")
      case number: Int => println(s"[simple actor] I have received a number: $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received something special: $contents")
      case SendMessageToYourself(content) => self ! content
      case SayHiTo(ref)                   => ref ! "Hi!"
      case WirelessPhoneMessage(content, ref) => ref forward (content + "s") // keeps the original sender of WirelessPhoneMessage
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"

  // 1 - messages can be of any type
  // a) messages must be IMMUTABLE
  // b) messages must be SERIALIZABLE

  // in practice use case classes and case objects
  simpleActor ! 42

  case class SpecialMessage(content: String)
  simpleActor ! SpecialMessage("some special content")

  // 2 - actors have information about their context and about themselves
  // context
  // context.self  (equivalent to "this")

  case class SendMessageToYourself(content: String)
  simpleActor ! SendMessageToYourself("I'm an actor and I am proud of it")

  // 3 - actors can REPLY to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  // 4 - deadLetter actor
  alice ! "Hi!" // reply to "me"

  // 5 - forwarding messages
  // forwarding = sending a message with the ORIGINAL sender
  case class WirelessPhoneMessage(content: String, ref: ActorRef)
  alice ! WirelessPhoneMessage("Hi", bob)


  /**
   * Counter actor:
   * - Increment
   * - Decrement
   * - Print
   */

  // DOMAIN of the counter
  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }
  class Counter extends Actor {
    import Counter._
    var count = 0
    override def receive: Receive = {
      case Increment => count += 1
      case Decrement => count -=1
      case Print => println(s"[counter] This is the count value: $count")
    }
  }

  val myCounter = system.actorOf(Props[Counter], "myCounter")
  for(_ <- 1 to 5) {
    myCounter ! Counter.Increment
  }
  for(_ <- 1 to 3) {
    myCounter ! Counter.Decrement
  }
  myCounter ! Counter.Print
}
