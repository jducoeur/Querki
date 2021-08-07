package querki.experiments

import querki.test._

/**
 * This class is the beginning of a proper memoized cache for use in Actors.
 *
 * The next step is to build a TimeoutMemo on top of this, that knows that it lives
 * inside of an Actor, which strobes periodically, and times values out after
 * they haven't been used for a while.
 */
class Memo[R, V]() {
  var memoizedValues: Map[R, V] = Map.empty

  def apply(received: R)(lookup: R => V): V = {
    val existing = memoizedValues.get(received)
    existing.getOrElse {
      val newVal = lookup(received)
      memoizedValues = memoizedValues + (received -> newVal)
      newVal
    }
  }
}

class MemoExperiments extends QuerkiTests {
  "Memo" should {
    "work with simple values" in {
      val memo = new Memo[Int, String]

      memo(1)(_ => "first value") should equal("first value")
      memo(2)(_ => "second value") should equal("second value")
      memo(1)(_ => "third value") should equal("first value")
    }

    "work with strings" in {
      val memo = new Memo[String, String]

      memo("one")(_ => "first value") should equal("first value")
      memo("two")(_ => "second value") should equal("second value")
      memo("one")(_ => "third value") should equal("first value")
    }

    "work with complex case classes" in {
      case class InnerCase(s2: String)
      case class MyClass(
        i: Int,
        s: String,
        inner: InnerCase
      )

      val memo = new Memo[MyClass, String]

      memo(MyClass(23, "one", InnerCase("a")))(_ => "first value") should equal("first value")
      memo(MyClass(42, "two", InnerCase("b")))(_ => "second value") should equal("second value")
      memo(MyClass(23, "one", InnerCase("a")))(_ => "third value") should equal("first value")
    }
  }
}
