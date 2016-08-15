import java.util.concurrent.Executor

sealed trait Eventual[+A] {
  def map[B](f: A => B)(implicit ex: Executor): Eventual[B] = flatMap(x => Eventual.of(f(x)))
  def onComplete(f: A => Unit)(implicit ex: Executor): Unit = map(f)

  def flatMap[B](f: A => Eventual[B])(implicit ex: Executor): Eventual[B]
  def get: Option[A]
}

object Eventual {

  // YOUR CODE HERE

  def apply[A](body: => A)(implicit ex: Executor): Eventual[A] = {
    // YOUR CODE HERE
  }

  def of[A](value: A)(implicit ex: Executor): Eventual[A] = {
    // YOUR CODE HERE
  }
}
