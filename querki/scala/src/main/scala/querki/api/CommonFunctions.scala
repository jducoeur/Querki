package querki.api

import querki.data.ThingInfo

trait CommonFunctions {
  /**
   * Fetches the "standard" info, which the Client needs to work with the Server.
   */
  def getStandardInfo():StandardInfo
  
  def getStandardThings():Map[String, ThingInfo]
}

/**
 * The "standard" info about the Server. This is a grab-bag of information that the Client fetches.
 */
case class StandardInfo(
  detailsPropId:String,
  summaryPropId:String,
  urPropId:String,
  namePropId:String,
  collPropId:String,
  typePropId:String,
  simpleThingId:String,
  isModelPropId:String,
  displayNamePropId:String,
  instancePropsPropId:String
)

/**
 * This is tricky magic. PassthroughHandler is defined completely differently on the Server
 * and Client, so that the Server mashalls and the Client unmarshalls from the same calls.
 *
 * IMPORTANT: this uses Names to find these values! This is more fragile than I like, but is
 * the tradeoff I'm making for the time being.
 */
class StandardThings(h:PassthroughHandler) {
  object core {
    val urProp = h.pass("Property")
    val nameProp = h.pass("Name")
    val collectionProp = h.pass("Property Collection")
    val typeProp = h.pass("Property Type")
    val isModelProp = h.pass("Is a Model")
  }
  
  object basic {
    val simpleThing = h.pass("Simple-Thing")
    val displayNameProp = h.pass("Display Name")
  }
  
  object conventions {
    val detailsProp = h.pass("Details")
    val summaryProp = h.pass("Summary")  
  }
  
  object editing {
    val instancePropsProp = h.pass("Instance Properties")
  }
  
  // This is necessary in order to force the objects to come into being. Each of the
  // above objects must be named here:
  def touchEverything() = {
    Seq(core, basic, conventions, editing)
  }
}
