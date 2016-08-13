import java.util.concurrent.Executor

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait Eventual[+A] {
  def flatMap[B](f: A => Eventual[B])(implicit ex: Executor): Eventual[B]
  def map[B](f: A => B)(implicit ex: Executor): Eventual[B] = flatMap(x => Eventual.of(f(x)))
  def onComplete(f: A => Unit)(implicit ex: Executor): Unit = map(f)
}

object Eventual {
  private case class Unresolved[A]() extends Eventual[A] {
    val callbacks = mutable.ListBuffer.empty[A => Runnable]

    override def flatMap[B](f: (A) => Eventual[B])(implicit ex: Executor) = {
      val resultWrapper = new Wrapper[B](new Unresolved[B], ex)
      synchronized {
        callbacks += { computed: A => new Runnable {
          override def run() = {
            val result = f(computed)
            resultWrapper.merge(result)
          }
        }}
      }
      resultWrapper
    }
  }

  private case class Computed[A](value: A) extends Eventual[A] {
    override def flatMap[B](f: (A) => Eventual[B])(implicit ex: Executor) = {
      val resultWrapper = new Wrapper[B](new Unresolved[B], ex)
      ex.execute(new Runnable {
        override def run() = {
          val result = f(value)
          resultWrapper.merge(result)
        }
      })
      resultWrapper
    }
  }

  private class Wrapper[A](_inner: Eventual[A], ex: Executor) extends Eventual[A] {
    @volatile private [Wrapper] var inner = _inner

    private [Eventual] def merge(other: Eventual[A]): Unit = synchronized {
      @inline @tailrec def merge0(o: Eventual[A]): Unit = (inner, o) match {
        case (w: Wrapper[A], _) =>
          inner = w.inner
          merge0(o)
        case (_, w: Wrapper[A]) => merge0(w.inner)
        case (mine@Unresolved(), other@Unresolved()) =>
          mine.callbacks ++= other.callbacks
        case (mine@Unresolved(), c@Computed(value)) =>
          inner = c
          fireCallbacks(mine, value)
        case (Computed(value), other@Unresolved()) =>
          fireCallbacks(other, value)
      }

      def fireCallbacks(container: Unresolved[A], value: A): Unit = {
        container.callbacks.foreach { f =>
          ex.execute(f(value))
        }
      }

      merge0(other)
    }

    override def flatMap[B](f: (A) => Eventual[B])(implicit ex: Executor) = inner.flatMap(f)
  }

  def apply[A](body: => A)(implicit ex: Executor): Eventual[A] = {
    val resultWrapper = new Wrapper[A](new Unresolved[A], ex)
    ex.execute(new Runnable {
      override def run() = resultWrapper.merge(Computed(body))
    })
    resultWrapper
  }

  def of[A](value: A): Eventual[A] = Computed(value)
}
