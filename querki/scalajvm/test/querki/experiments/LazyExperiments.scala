package querki.experiments

import querki.test._

class LazyExperiments extends QuerkiTests {
  "A case class" should {
    "re-evaluate its lazy vals when copied" in {
      case class MyClass(nums: Seq[Int]) {
        lazy val total: Int = nums.sum
      }

      val first = MyClass(Seq(1, 2, 3))
      assert(first.total == 6)

      val second = first.copy(nums = first.nums :+ 4)
      assert(second.total == 10)
    }
  }
}
