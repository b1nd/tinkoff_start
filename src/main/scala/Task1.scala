import java.io.{BufferedWriter, OutputStreamWriter}
import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.immutable.Stream.#::

object Task1 {

  implicit class IntExtensions(i: Int) {
    def **(exp: Int): Int = {
      @tailrec def power(res: Int, p: Int): Int = p match {
        case 0 => 1
        case 1 => res
        case _ => power(res * i, p - 1)
      }

      power(i, exp)
    }
  }

  @tailrec def writeNice(i: Int, c: Int, write: Int => Unit, writeSpace: => Unit): Unit = if (c > 1) {
    if ((i / (10 ** (c - 1))) % 10 == 0) {
      writeSpace
      writeNice(i, c - 1, write, writeSpace)
    } else write(i)
  } else write(i)

  // O(n * m * c)
  def go(n: Int, m: Int, c: Int, write: Int => Unit, writeSpace: => Unit, writeEmpty: => Unit): Unit =
    0 until n foreach { r =>
      val asc = Range(r * m, (r + 1) * m)
      val row = if (r % 2 == 0) asc else asc.reverse

      row.foreach(i => writeNice(i, c, write, writeSpace))
      writeEmpty // skip if last?
    }

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val out = new BufferedWriter(new OutputStreamWriter(System.out))

    val n #:: m #:: _ = Stream.continually(in.nextInt).take(2)

    in.close()

    go(n, m, 3, i => out.write(i.toString), out.write(' '), out.newLine())

    out.close()
  }
}
