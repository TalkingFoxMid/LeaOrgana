package github.talkingfox.lea


import github.talkingfox.lea.Lea.{Fiber, Join, Lea, Pure}

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.collection.mutable._

object Runtime {
  trait Klee {
    def runLeaSync[A](lea: Lea[A]): A
  }

  class SimpleKlee() extends Klee {


    override def runLeaSync[A](lea: Lea[A]): A = {
      val queue = new ArrayBlockingQueue[A](1)
      new SimpleFiber[A](lea, queue.put).runAsync
      queue.take()
    }
  }

  class SimpleFiber[A](lea: Lea[A], cb: A => Unit) extends Fiber[A] {
    val queue = new ArrayBlockingQueue[A](1)
    val ec = ExecutionContext.global
    val completed: AtomicReference[Option[A]] = new AtomicReference[Option[A]](None)

    val conts = new ArrayBuffer[A => Unit]
    conts.addOne(
      a => completed.set(Some(a))
    )

    def addCont(cont: A => Unit): Unit = {
      synchronized(conts) {
        conts.addOne(cont)
        completed.get().foreach(cont)
        0
      }
    }
    def runAsync: Unit = runLoop[A](lea.asInstanceOf[Lea[Any]], Nil, a => {
      cb(a)
      queue.put(a)
      completed.set(Some(a))
      synchronized(conts) {
        conts.foreach(
          cont => cont(a)
        )
        0
      }
    })


    def runLoop[B](cur: Lea[Any], conts: List[Any => Lea[Any]], suc: B => Unit): Unit =
      cur match {
        case Lea.Pure(a) => conts match {
          case head :: tail => runLoop[B](head(a), tail, suc)
          case Nil => suc(a.asInstanceOf[B])
        }
        case Lea.Delay(a) => runLoop(Lea.Pure(a()), conts, suc)
        case Lea.FlatMap(a, f) => runLoop(a, f :: conts, suc)
        case Lea.Start(s: Lea[Any]) => {
          val fiber = new SimpleFiber[Any](s, a => ())
          ec.execute(() => fiber.runAsync)
          runLoop[B](Lea.Pure(fiber).asInstanceOf[Lea[Any]], conts, suc)
        }
        case Lea.Join(fiber) => {
          val fiberTyped = fiber.asInstanceOf[SimpleFiber[Any]]
          fiberTyped.addCont(result => runLoop[B](Pure(result).asInstanceOf[Lea[Any]], conts, suc))
        }
        case Lea.Async(cb) => {
          val res = cb(b => ec.execute(() => runLoop(Pure(b), conts, suc)))
          runLoop[Unit](res.asInstanceOf[Lea[Any]], Nil, _ => ())
        }
      }

    var canceled: AtomicBoolean = new AtomicBoolean(false)

    override def cancel: Lea[Unit] = Lea.Delay(() => canceled.set(true))

    override def join: Lea[A] = Join(this)

    def joinSync: A = queue.poll()
  }


}
