import java.util.Scanner

import scala.annotation.tailrec
import scala.math.abs

object Task3 {

  // O(n + m)
  @tailrec def go(a: List[Int], b: List[Int], pair: (Int, Int)): (Int, Int) = if (a == Nil || b == Nil) {
    pair
  } else {
    val p = abs(a.head - b.head) match {
      case r if r < abs(pair._1 - pair._2) => (a.head, b.head)
      case _ => pair
    }
    if (p._1 < p._2) {
      go(a.tail, b, p)
    } else {
      go(a, b.tail, p)
    }
  }

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)

    val n = in.nextInt
    val a = List.tabulate(n)(_ => in.nextInt)

    val m = in.nextInt
    val b = List.tabulate(m)(_ => in.nextInt)

    in.close()

    val (p1, p2) = go(a, b, (-1000001, 1000001))

    print(p1)
    print(' ')
    print(p2)
  }
}
