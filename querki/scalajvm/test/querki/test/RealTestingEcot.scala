package querki.test

import querki.globals._

trait TestingControls {
  def getEvents(): List[Any]
  def clearEvents(): Unit
}

class RealTestingEcot(e: Ecology) extends QuerkiEcot(e) with Testing with TestingControls {

  /**
   * Accumulated test data.
   *
   * @param events the list of events since last cleared, in reverse order
   */
  case class TestData(events: List[Any])

  // TODO: this should really be an AtomicReference, or better yet a Ref, but we need to upgrade the whole
  // stack a ways before that works easily:
  var testData: TestData = TestData(List.empty)

  def happened(event: => Any): Unit = synchronized {
    val e: Any = event
    testData = testData.copy(events = e :: testData.events)
  }

  def getEvents() = testData.events.reverse

  def clearEvents() = synchronized {
    testData = testData.copy(events = List.empty)
  }
}
