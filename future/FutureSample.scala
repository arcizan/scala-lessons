import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object FutureSample extends App {
  def sample1: Unit = {
    def foo: Future[Int] = Future {
      Thread.sleep(1000)
      println("sample1: in foo")
      1
    }

    def bar: Unit = {
      val f = foo
      f.onSuccess { case x =>
        Thread.sleep(1000)
        println("sample1: in onSuccess", x)
      }
      println("sample1: in bar")
    }

    bar
    println("sample1: at last")
  }

  def sample2: Unit = {
    def foo: Future[Int] = Future {
      Thread.sleep(1000)
      println("sample2: in foo")
      1
    }

    def bar: Unit = {
      val f = foo
      val f2 = f.map { x =>
        Thread.sleep(1000)
        println("sample2: in map", x)
      }
      println("sample2: in bar")
      Await.ready(f2, 2000 millisecond)
    }

    bar
    println("sample2: at last")
  }

  def sample3: Unit = {
    def foo(i: Int): Future[Int] = Future {
      Thread.sleep(1000)
      println("sample3: in foo")
      i
    }

    def bar(i: Int): Future[Unit] = {
      val f = foo(i)
      val f2 = f.map { x =>
        Thread.sleep(1000)
        println("sample3: in map", x)
      }
      println("sample3: in bar")
      f2
    }

    val fs: Seq[Future[Unit]] = (1 to 10).toSeq.map(x => bar(x))
    val f: Future[Seq[Unit]] = Future.sequence(fs)

    Await.ready(f, Duration.Inf)
    println("sample3: at last")
  }

  // sample1
  // sample2
  sample3
}
