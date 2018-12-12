package querki.test.mid

import querki.data._

import AllFuncs._

trait PropFuncs {
  /**
   * The Collections, for use when creating a new Property.
   */
  def exactlyOne: TestOp[ThingInfo] = fetchStd(_.core.exactlyOneColl)
  def optional: TestOp[ThingInfo] = fetchStd(_.core.optionalColl)
  def qlist: TestOp[ThingInfo] = fetchStd(_.core.listColl)
  def qset: TestOp[ThingInfo] = fetchStd(_.core.setColl)
  
  def makeProperty(name: String, collOp: TestOp[ThingInfo], tpeOp: TestOp[ThingInfo]): TestOp[TID] = {
    for {
      coll <- collOp
      tpe <- tpeOp
      collProp <- fetchStd(_.core.collectionProp)
      tpeProp <- fetchStd(_.core.typeProp)
      propModel <- fetchStd(_.core.urProp)
      propId <- makeThing(propModel, name,
        tpeProp :=> tpe,
        collProp :=> coll
      )
    }
      yield propId
  }
}

object PropFuncs extends PropFuncs
