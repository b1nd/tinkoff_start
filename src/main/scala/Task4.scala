import java.io.{BufferedWriter, OutputStreamWriter}
import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.immutable.Stream.#::

object Task4 {

  // Arithmetic progression nth element
  def nth(init: Int, d: Int, n: Int): Int = init + d * (n - 1)

  // O(n)
  @tailrec def go(a: Int, x: Int, n: Int, write: Int => Unit, writeSpace: => Unit): Unit = if (n > 0) {
    write(nth(a, x, n))
    writeSpace // skip if last?
    go(a, x, n - 1, write, writeSpace)
  }

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val out = new BufferedWriter(new OutputStreamWriter(System.out))

    val a #:: x #:: n #:: _ = Stream.continually(in.nextInt).take(3)

    in.close()

    go(a, x, n, i => out.write(i.toString), out.write(' '))

    out.close()
  }
}
