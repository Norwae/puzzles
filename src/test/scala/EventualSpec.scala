import java.util.UUID
import java.util.concurrent.{Executor, Executors}

import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Random

class EventualSpec extends FlatSpec with Matchers with Eventually with BasicAsyncBehaviours with BeforeAndAfterAll {
  val singleThread = Executors.newSingleThreadExecutor()
  val pool = Executors.newFixedThreadPool(10)

  "Basic async functionality all on one thread" should behave like basicAsyncImplementation(SameThreadExecutor)
  it should behave like sequentialAsync(SameThreadExecutor)

  "A single external thread" should behave like basicAsyncImplementation(singleThread)
  it should behave like sequentialAsync(singleThread)

  "true multi-threading" should behave like basicAsyncImplementation(pool)

  it should "be significantly faster than serial execution" in {
    val deadline = Deadline.now + 500.millis
    implicit val ex = pool
    val first, second, third, fourth, fifth = Eventual(slowRandomInt())
    val sum = List(first, second, third, fourth, fifth).foldLeft(Eventual.of(0))((a, b) => a.flatMap(aVal => b.map(_ + aVal)))

    eventually {
      sum.get.get should (be >= 5 and be <= 45)
    }
    deadline should not be 'overdue
  }

  override protected def afterAll() = {
    singleThread.shutdownNow()
    pool.shutdownNow()
    super.afterAll()
  }
}

trait BasicAsyncBehaviours { self: FlatSpec with Matchers with Eventually =>
  override implicit def patienceConfig = PatienceConfig(timeout = Span(45, Seconds))

  def slowRandomUUID() = {
    Thread.sleep(250)
    UUID.randomUUID()
  }

  def slowRandomInt() = {
    Thread.sleep(250)
    Random.nextInt(9) + 1
  }

  def sequentialAsync(implicit ex: Executor): Unit = {
    it should "finish a a simple call in reasonable time" in {
      val deadline = Deadline.now + 300.millis
      val ev = Eventual(slowRandomUUID())
      eventually(ev.get should not be empty)
      deadline should not be 'overdue
    }

    it should "take longer than its component operations" in {
      val deadline = Deadline.now + 700.millis
      val first, second, third = Eventual(slowRandomInt())
      val combined = for {
        f <- first
        s <- second
        t <- third
      } yield f + s + t

      eventually(combined.get.get should (be >= 3 and be <= 27))
      deadline should be ('overdue)
    }
  }

  def basicAsyncImplementation(implicit executor: Executor): Unit = {
    it should "successfully convert a known value into an Eventual" in {
      val known = UUID.randomUUID()
      Eventual.of(known).get shouldEqual Some(known)
    }

    it should "allow deferring a computation into an Eventual" in {
      val ev = Eventual(slowRandomUUID())
      eventually {
        ev.get should not be empty
      }
    }

    it should "support the map() operation" in {
      val ev = Eventual(slowRandomInt())
      val mapped = ev.map(_ * 2).map(_ + 5)

      eventually {
        mapped.get should contain oneOf(7, 9, 11, 13, 15, 17, 19, 21, 23)
      }
    }

    it should "support the flatMap operation" in {
      val ev = Eventual(slowRandomUUID())

      val mapped = ev.flatMap(_ => Eventual(slowRandomInt())).flatMap(_ => Eventual(19))

      eventually {
        mapped.get should contain (19)
      }
    }

    it should "support mixing operations" in {
      val ev1 = Eventual(slowRandomInt())
      val ev2 = Eventual(slowRandomInt())
      val ev3 = Eventual(slowRandomInt())

      val mapped = for {
        h <- ev1
        hundred = h * 100
        t <- ev2
        ten = t * 10
        one <- ev3
      } yield hundred + ten + one

      eventually {
        mapped.get.get should (be >= 100 and be <= 999)
      }
    }

    it should "allow forking computations" in {
      val ev1 = Eventual(slowRandomInt())
      val ev2 = ev1.map(_ * 10)
      val ev3 = ev1.map(_ + 10)

      eventually {
        ev2.get should contain oneOf(10, 20, 30, 40, 50, 60, 70, 80, 90)
        ev3.get should contain oneOf(11, 12, 13, 14, 15, 16, 17, 18, 19)
      }
    }

    it should "be resistant against many, interleaving computations (simple concurrency check)" in {
      val basics = List.fill(100)(Eventual(slowRandomInt().toLong))
      val pairs = basics map { eventual =>
        val ev1 = eventual.flatMap { int =>
          Eventual((int, int * 2))
        }
        val ev2 = eventual.map(-_)
        eventual.onComplete(Thread.sleep)

        ev2.flatMap(_ => ev1)
      }

      val tested = basics.zip(pairs).map { outerPair =>
        val (single, tuple) = outerPair
        for {
          valuePair <- tuple
          value <- single
        } yield value == valuePair._1 && 2 * value == valuePair._2
      }

      eventually {
        tested.forall(_.get.get)
      }
    }
  }

}
