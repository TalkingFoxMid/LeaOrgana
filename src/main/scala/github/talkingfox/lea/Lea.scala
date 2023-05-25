package github.talkingfox.lea


import java.util.Timer
import java.util.concurrent.ScheduledExecutorService
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

object Lea {

  import java.util.concurrent.Executors
  import java.util.concurrent.ScheduledExecutorService

  private val scheduler = Executors.newScheduledThreadPool(1)
  def sleep(dur: FiniteDuration): Lea[Unit] =
    Async[Unit](
      cb => Delay(() =>scheduler.schedule(
        () => {
          () => cb(())
        }, dur.toNanos, NANOSECONDS
      ))
    )
  sealed trait Lea[+A] {
    def flatMap[B](f: A => Lea[B]): Lea[B] =
      FlatMap[B](this, x => f(x.asInstanceOf[A]).map(x => x: Any))

    def map[B](f: A => B): Lea[B] =
      FlatMap[B](this, x => Pure(f(x.asInstanceOf[A])))

    def start: Lea[Fiber[A]] = {
      Start(this)
    }

  }
  case class Pure[A](a: A) extends Lea[A]
  case class Delay[A](a: () => A) extends Lea[A]
  case class FlatMap[A](a: Lea[Any], f: Any => Lea[Any]) extends Lea[A]
  case class Start[A](lea: Lea[A]) extends Lea[Fiber[A]]
  case class Join[A](fiber: Fiber[A]) extends Lea[A]
  case class Async[A](cb: (A => Unit) => Lea[Unit]) extends Lea[A]


  trait Fiber[+A] {
    def cancel: Lea[Unit]
    def join: Lea[A]
  }

}
