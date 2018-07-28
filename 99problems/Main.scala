object Main {
  def last[T](xs: List[T]): T = xs.last

  def penultimate[T](xs: List[T]): T = xs.init.last

  def nth[T](index: Int, xs: List[T]): T = xs.apply(index)

  def length[T](xs: List[T]): Int = xs.length

  def reverse[T](xs: List[T]): List[T] = xs.reverse

  def isPalindrome[T](xs: List[T]): Boolean = xs == xs.reverse

  def flatten(xs: List[Any]): List[Any] = {
    xs.flatMap {
      case i:List[_] => flatten(i)
      case i => List(i)
    }
  }

  def compress[T](xs: List[T]): List[T] = {
    xs match {
      case h :: t => h :: compress(t.dropWhile(_ == h))
      case Nil => Nil
    }
  }

  def pack[T](xs: List[T]): List[List[T]] = {
    xs match {
      case h :: t => xs.takeWhile(_ == h) :: pack(t.dropWhile(_ == h))
      case Nil => Nil
    }
  }

  def encode[T](xs: List[T]): List[(Int, T)] = {
    pack(xs).map(x => (x.size, x.head))
  }

  def encodeModified[T](xs: List[T]): List[Any] = {
    encode(xs).map {
      case (1, x) => x
      case x => x
    }
  }

  def decode[T](xs: List[(Int, T)]): List[T] = {
    xs.flatMap(x => List().padTo(x._1, x._2))
  }

  def encodeDirect[T](xs: List[T]): List[(Int, T)] = {
    xs match {
      case h :: t => (xs.takeWhile(_ == h).size, h) :: encodeDirect(t.dropWhile(_ == h))
      case Nil => Nil
    }
  }

  def duplicate[T](xs: List[T]): List[T] = {
    xs.flatMap(x => List(x, x))
  }

  def duplicateN[T](n: Int, xs: List[T]): List[T] = {
    xs.flatMap(x => List().padTo(n, x))
  }

  def drop[T](n: Int, xs: List[T]): List[T] = {
    xs match {
      case _ :: _ => xs.take(n-1) ++ drop(n, xs.drop(n))
      case Nil => Nil
    }
  }

  def split[T](n: Int, xs: List[T]): (List[T], List[T]) = xs.splitAt(n)

  def slice[T](from: Int, until: Int, xs: List[T]): List[T] = xs.slice(from, until)

  def rotate[T](n: Int, xs: List[T]): List[T] = {
    if( n < 0 ){
      xs.takeRight(-n) ++ xs.dropRight(-n)
    }else if( n > 0 ){
      xs.drop(n) ++ xs.take(n)
    }else{
      xs
    }
  }

  def removeAt[T](n: Int, xs: List[T]): (List[T], T) = {
    (xs.take(n) ++ xs.drop(n+1), xs.apply(n))
  }
}
