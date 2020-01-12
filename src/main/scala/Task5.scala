import java.util.Scanner

import scala.annotation.tailrec

object Task5 {

  // Klee’s Algorithm - O(n log n)
  def algo(pairs: List[(Int, Int)]): Int = {
    val points = pairs flatMap { case (p1, p2) => List((p1, false), (p2, true)) } sortBy { case (p, _) => p }

    @tailrec def go(points: List[(Int, Boolean)], prev: Int = 0, c: Int = 0, len: Int = 0): Int = points match {
      case (p, end) :: tail =>
        val diff = if (c != 0) p - prev else 0
        val newLen = len + diff

        if (end) go(tail, p, c - 1, newLen)
        else go(tail, p, c + 1, newLen)
      case Nil => len
    }

    go(points)
  }

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)

    val n = in.nextInt
    val a = List.tabulate(n)(_ => (in.nextInt, in.nextInt))

    print(algo(a))
  }
}
