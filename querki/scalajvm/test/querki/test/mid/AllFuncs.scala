package querki.test.mid

/**
 * The master object with all the helpful mid-test functions. You can simply import all of this to
 * get everything.
 */
trait AllFuncs
  extends ApiFuncs
     with ClientFuncs
     with CoreFuncs
     with EditFuncs
     with FormFuncs
     with LoginFuncs
     with PropFuncs
     with SpaceFuncs
     with ThingFuncs

object AllFuncs extends AllFuncs
