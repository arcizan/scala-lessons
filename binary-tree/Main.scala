object Main {
  trait Node {
    val v: Int

    def size(): Int
    def max(): Int
    def min(): Int
    def sum(): Int
    def avg(): Double
    def find(n: Int): Option[Node]
  }

  case class Branch(l: Node, v: Int, r: Node) extends Node {
    require(l.v < v && r.v > v)

    def size(): Int = l.size + 1 + r.size
    def max(): Int = r.max
    def min(): Int = l.min
    def sum(): Int = l.sum + v + r.sum
    def avg(): Double = sum.toDouble / size
    def find(n: Int): Option[Node] = {
      n match {
        case n if n == v => Some(this)
        case n if n < v => l.find(n)
        case n if n > v => r.find(n)
      }
    }
  }

  case class Leaf(v: Int) extends Node {
    def size(): Int = 1
    def max(): Int = v
    def min(): Int = v
    def sum(): Int = v
    def avg(): Double = v.toDouble
    def find(n: Int): Option[Node] = {
      n match {
        case n if n == v => Some(this)
        case _ => None
      }
    }
  }

  case class BTree(node: Node) {
    def size(): Int = node.size
    def max(): Int = node.max
    def min(): Int = node.min
    def sum(): Int = node.sum
    def avg(): Double = node.avg
    def find(n: Int): Option[Node] = node.find(n)
  }

  implicit class MyList(val xs: List[Int]) extends AnyVal {
    def toBTree(): BTree = {
      def _toBTree(xs: List[Int]): Node = {
        xs.size match {
          case n if n == 1 => Leaf(xs(0))
          case n if n % 2 == 1 => {
            val (l, r) = xs.splitAt(n / 2)
            Branch(_toBTree(l), r(0), _toBTree(r.tail))
          }
        }
      }
      BTree(_toBTree(xs))
    }
  }
}
