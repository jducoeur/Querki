package querki.apps

import querki.persistence._
import querki.spaces._
import querki.test._

class AppSnapshotTests extends QuerkiTests with AppTree {

  // TODO: this is necessary because the lazy vals in CoreAppTests -- highest, mid1, mid2, etc -- are ad-hoc
  // structurally-defined objects, so we're reaching into them structurally to access them. That's just plain
  // dumb: it's bad style, and inefficient. Figure out a way to rewrite them as proper classes/objects instead.
  // Ditto for the "original" value below.
  import scala.language.reflectiveCalls

  def runSnapshotTest(interval: Int) = {
    val original = new SpaceInWorldWithSnapshots(highest, interval) {
      addApp(highest)

      val inst1 = addThing("Instance 1", highest.rootModel)
      val inst2 = addThing("Instance 2", highest.rootModel)
      val inst3 = addThing("Instance 3", highest.rootModel)
      val inst4 = addThing("Instance 4", highest.rootModel)
      val inst5 = addThing("Instance 5", highest.rootModel)
    }
    implicit val replay = new ReplayCoreSpace(original)
    implicit val s = replay.state

    pql("""[[My Root Model._instances]]""") should
      equal(listOfLinkText(
        highest.highestInstance,
        original.inst1,
        original.inst2,
        original.inst3,
        original.inst4,
        original.inst5
      ))
  }

  "Snapshotting" should {
    "work with Apps" in {
      runSnapshotTest(3)
      runSnapshotTest(8)
    }
  }
}

class AppPersistenceTests(env: PersistEnv) extends PersistTest(env) with SpaceMessagePersistenceBase with AppTree {
  import SpaceMessagePersistence._

  lazy val Basic = interface[querki.basic.Basic]

  val s = mainSpace
  implicit val state = s.state

  checkSerialization(dh(state))
  checkSerialization(SpaceSnapshot(dh(state), Seq.empty))
  checkSerialization(SpaceSnapshot(dh(state), state.allApps.values.toSeq.map(dh(_))))
}
