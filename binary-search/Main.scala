object Main {
  import scala.util.{Failure,Try,Success}

  def bsearch(n: Int, xs: Vector[Int]): Try[Int] = {
    def _bsearch(l: Int, r: Int): Try[Int] = {
      if(l < r){
        (l + r) / 2 match {
          case m if xs(m) == n => Success(m)
          case m if xs(m) > n => _bsearch(l, m-1)
          case m if xs(m) < n => _bsearch(m+1, r)
        }
      }else{
        xs(l) match {
          case x if x == n => Success(l)
          case _ => Failure(new NoSuchElementException)
        }
      }
    }
    _bsearch(0, xs.size - 1)
  }

  def qsort(xs: Vector[Int]): Vector[Int] = {
    xs match {
      case Vector() => xs
      case Vector(x, _*) => {
        val (l, t) = xs.partition(_ < x)
        val (u, s) = t.partition(_ > x)
        qsort(l) ++ s ++ qsort(u)
      }
    }
  }
}
