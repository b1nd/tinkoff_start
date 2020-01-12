import scala.annotation.tailrec
import scala.collection.immutable.Stream.#::
import scala.io.StdIn
import scala.math.max

// В одном месте сказано монотонная, а в другом строго монотонная, судя по ответу все-таки строго
object Task2 {

  // O(n)
  @tailrec def go[T](list: Stream[T], op: (T, T) => Boolean, invOp: (T, T) => Boolean, same: Int = 0, cur: Int = 0, _max: Int = 0): Int = list match {
    case Stream() => 0
    case head #:: next #:: tail if head == next   => go(next #:: tail, op, invOp, same + 1, cur + 1, _max)
    case head #:: next #:: tail if op(head, next) => go(next #:: tail, op, invOp, 0, cur + 1, _max)
    case _    #:: next #:: tail                   => go(next #:: tail, invOp, op, 0,  same + 1, max(cur, _max))
    case _ => max(cur, _max) + 1
  }

  // O(n)
  @tailrec def goStrict[T](list: Stream[T], op: (T, T) => Boolean, invOp: (T, T) => Boolean, cur: Int = 0, _max: Int = 0): Int = list match {
    case Stream() => 0
    case head #:: next #:: tail if head == next   => goStrict(next #:: tail, op, invOp, 0, max(cur, _max))
    case head #:: next #:: tail if op(head, next) => goStrict(next #:: tail, op, invOp, cur + 1, _max)
    case _    #:: next #:: tail                   => goStrict(next #:: tail, invOp, op, 1, max(cur, _max))
    case _ => max(cur, _max) + 1
  }

  def main(args: Array[String]): Unit = {
    val a = Stream.continually(StdIn.readInt).takeWhile(_ != 0)

    def op(a: Int, b: Int): Boolean = a > b
    def invOp(a: Int, b: Int): Boolean = a < b

    print(goStrict(a, op, invOp))
  }
}
