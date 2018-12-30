package querki.test.mid

import querki.conversations.ConvMidFuncs

/**
 * The master object with all the helpful mid-test functions. You can simply import all of this to
 * get everything.
 */
trait AllFuncs
  extends ApiFuncs
    with ClientFuncs
    with ConvMidFuncs
    with CoreFuncs
    with EditFuncs
    with FormFuncs
    with LoginFuncs
    with PropFuncs
    with SetupFuncs
    with SpaceFuncs
    with ThingFuncs

object AllFuncs extends AllFuncs
