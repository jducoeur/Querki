package querki.ql

import com.github.nscala_time.time.Imports._

import querki.ecology.{Ecology, EcotIds}
import querki.globals._
import querki.test.QuerkiTests

class TimeoutTests extends QuerkiTests {

  lazy val LongType = Core.LongType

  object TimeoutMOIDs extends EcotIds(1000) {
    val SetTimeOID = moid(1)
  }

  class TimeHookEcot(e: Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
    import TimeoutMOIDs._

    lazy val setTimeMethod = new InternalMethod(SetTimeOID,
      toProps(
        setName("_setTime")
      ))
    {
      override def qlApply(inv: Invocation): QFut = {
        for {
          dateLong <- inv.contextAllAs(LongType)
          dateTime = new DateTime(dateLong)
          _ = setTime(dateTime)
        }
          yield ExactlyOne(LongType(dateLong))
      }
    }

    override lazy val props = Seq(
      setTimeMethod
    )
  }

  override def createEcots(e:Ecology): Unit = {
    super.createEcots(e)

    new TimeHookEcot(e)
  }

  "QL Timeouts" should {
    "work" in {
      // This test uses the _setTime function injected above. In mid-stream, it sets the clock time to six seconds
      // after we started, and the test implementation of TimeProvider (in QuerkiTests) sets the maximum run time
      // to five seconds. So we expect the later stages of the expression to time out:
      implicit val s = commonSpace
      setTime(new DateTime(1560980782000L))

      pql("""[[Trivial Thing -> 1560980788000 -> _setTime -> ""hello"" -> _textLength]]""") should
        equal (expectedWarning("QL.timeout"))
    }
  }
}
