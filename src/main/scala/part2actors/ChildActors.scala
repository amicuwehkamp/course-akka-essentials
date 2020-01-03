package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChildActors.CreditCard.{AttachToAccount, CheckStatus}
import part2actors.ChildActors.Parent.{CreateChild, TellChild}

object ChildActors extends App {

  // Actors can create other actors

  object Parent {

    case class CreateChild(name: String)

    case class TellChild(message: String)

  }

  class Parent extends Actor {

    import Parent._

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"${self.path} creating child")
        // create actor here:
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(childRef: ActorRef): Receive = {
      case TellChild(message) => childRef forward message
    }
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"${self.path} I got: $message")
    }
  }

  val system = ActorSystem("ParentChildDemo")
  val parent = system.actorOf(Props[Parent], "parent")
  parent ! CreateChild("child")
  parent ! TellChild("hey kid!")

  // child hierarchies
  // parent -> child -> grandchild

  /*
  Guardian actors (top-level)
  - /system = system guardian
  - /user = user-level guardian
  - / = the root guardian
   */

  /*
  Actor selection
   */
  val childSelection = system.actorSelection("/user/parent/child")
  childSelection ! "I found you!"

  /*
  Danger!

  NEVER PASS MUTABLE ACTOR STATE, OR THE 'THIS' REFERENCE, TO CHILD ACTORS!
   */

  object NaiveBankAccount {

    case class Deposit(amount: Int)

    case class Withdraw(amount: Int)

    case object InitializeAccount

  }

  class NaiveBankAccount extends Actor {

    import NaiveBankAccount._
    import CreditCard._

    var amount = 0

    override def receive: Receive = {
      case InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard], "card")
        creditCardRef ! AttachToAccount(this) //// NO NO NO!
      case Deposit(funds)    => deposit(funds)
      case Withdraw(funds)   => withdraw(funds)
    }

    def deposit(funds: Int) = {
      println(s"${self.path} depositing $funds on top of $amount")
      amount += funds
    }

    def withdraw(funds: Int) = {
      println(s"${self.path} withdrawing $funds from $amount")
      amount -= funds
    }

  }

  object CreditCard {

    case class AttachToAccount(bankAccount: NaiveBankAccount) // NOOOOO!  (use the ActorRef, not the actor itself)
    case object CheckStatus
  }

  class CreditCard extends Actor {

    override def receive: Receive = {
      case AttachToAccount(account) => context.become(attachedTo(account))
    }

    def attachedTo(account: NaiveBankAccount): Receive = {
      case CheckStatus =>
        println(s"${self.path} your message has been processed.")
        account.withdraw(1)  /// IT'S POSSIBLE AND IT'S NOT OK
    }
  }

  import NaiveBankAccount._
  import CreditCard._

  val bankAccountRef = system.actorOf(Props[NaiveBankAccount], "account")
  bankAccountRef ! InitializeAccount
  bankAccountRef ! Deposit(500)

  Thread.sleep(500)
  val creditCardSelection = system.actorSelection("/user/account/card")
  creditCardSelection ! CheckStatus
}
