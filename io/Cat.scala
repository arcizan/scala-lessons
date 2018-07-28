import scala.io.Source

object Cat extends App {
  if (args.size == 0) {
    Source.stdin.getLines.foreach(println)
  } else {
    // args.foreach(f => using(Source.fromFile(f))(_.getLines.foreach(println)))
    val lines = args.flatMap(f => using(Source.fromFile(f))(_.getLines.toList)) // stream が閉じられた後で行を使用する場合は toList などで変換する必要がある
    lines.foreach(println)
  }

  def using[A <: {def close()}, B](resource: A)(f: A => B): B = {
    try f(resource)
    finally {
      try resource.close
      catch { case _: Throwable => }
    }
  }
}
