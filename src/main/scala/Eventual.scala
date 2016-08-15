import java.util.concurrent.Executor

import scala.collection.mutable

sealed trait Eventual[+A] {
  def map[B](f: A => B)(implicit ex: Executor): Eventual[B] = flatMap(x => Eventual.of(f(x)))
  def onComplete(f: A => Unit)(implicit ex: Executor): Unit = map(f)

  def flatMap[B](f: A => Eventual[B])(implicit ex: Executor): Eventual[B]
  def get: Option[A]
}

object Eventual {
  case class MutableEventual[A](ex: Executor) extends Eventual[A] {
    private class RunnableAdapter(f: A => Unit, value: A) extends Runnable {
      override def run() = f(value)
    }

    private sealed trait ComputationState {
      def doWithValue(f: A => Unit): Unit
      def get: Option[A]
    }

    private case class Unresolved() extends ComputationState {
      val callbacks = mutable.ListBuffer.empty[A => Runnable]

      override def doWithValue(f: (A) => Unit) = synchronized {
        callbacks += { value: A => new RunnableAdapter(f, value)}
      }

      def fireCallbacks(value: A) = {
        callbacks.foreach(functor => ex.execute(functor(value)))
      }

      override def get = None
    }

    private case class Resolved(value: A) extends ComputationState {
      override def doWithValue(f: (A) => Unit) = ex.execute(new RunnableAdapter(f, value))

      override def get = Some(value)
    }

    private var state: ComputationState = Unresolved()

    private def doWithValue(f: A => Unit) = synchronized {
      state.doWithValue(f)
    }

    def resolve(value: A) = synchronized {
      state.asInstanceOf[Unresolved].fireCallbacks(value)
      state = Resolved(value)
    }

    def get = state.get

    override def flatMap[B](f: (A) => Eventual[B])(implicit ex: Executor) = {
      val resultWrapper = new MutableEventual[B](ex)
      doWithValue { value =>
        val result = f(value)
        result.asInstanceOf[MutableEventual[B]].doWithValue(resultWrapper.resolve)
      }
      resultWrapper
    }
  }

  def apply[A](body: => A)(implicit ex: Executor): Eventual[A] = {
    val resultWrapper = new MutableEventual[A](ex)
    ex.execute(new Runnable {
      override def run() = resultWrapper.resolve(body)
    })
    resultWrapper
  }

  def of[A](value: A)(implicit ex: Executor): Eventual[A] = {
    val resultWrapper = new MutableEventual[A](ex)
    resultWrapper.resolve(value)
    resultWrapper
  }
}
