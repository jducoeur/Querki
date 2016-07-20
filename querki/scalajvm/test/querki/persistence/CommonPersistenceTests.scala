package querki.persistence

import querki.time.DateTime

/**
 * This tests persistence roundtrips of some of the basic core types.
 */
class CommonPersistenceTests(env:PersistEnv) extends PersistTest(env) {
  // DateTime
  checkRaw(DateTime.now)
  checkRaw(new DateTime(0))
}
