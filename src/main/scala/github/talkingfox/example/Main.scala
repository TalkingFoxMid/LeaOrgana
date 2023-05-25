package github.talkingfox.example


import github.talkingfox.lea.Lea
import github.talkingfox.lea.Lea.{Delay, FlatMap, Lea, Pure, Start}
import github.talkingfox.lea.Runtime.SimpleKlee
import scala.concurrent.duration._
object Main extends App {

  def waitAndPrint: Lea[Unit] =
    Lea.sleep(1.seconds).flatMap(_ => Delay(() => println("HM")))
  val klee = new SimpleKlee
  println(klee.runLeaSync(waitAndPrint))
}
