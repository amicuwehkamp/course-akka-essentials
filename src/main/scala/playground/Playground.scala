package playground

import akka.actor.ActorSystem

object Playground extends App {

  val actorSystem = ActorSystem("HelloAkka")
  println(actorSystem.name)

  class B[_]
  class A {
    def doStuff(implicit b: B[A]): Unit = {
    }
  }
  object A {
    implicit val myVal: B[A] = new B[A]
  }

  def takeAImplicitly(a: A): A = a

  takeAImplicitly(new A).doStuff
}
