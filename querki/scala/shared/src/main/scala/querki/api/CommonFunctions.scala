package querki.api

import scala.concurrent.Future

import querki.data.ThingInfo

trait CommonFunctions {
  /**
   * Pro-actively fetches the "standard" Things (from System Space) that the Client cares about.
   * This list is pretty ad-hoc, but it's useful to get them in advance.
   *
   * Note that, on the Client, this is actually exposed as the StandardThings structure. But that
   * gets serialized as a Map for going across the wire.
   */
  def getStandardThings():Map[String, ThingInfo]
  
  /**
   * Check on the progress of a long-running Operation.
   * 
   * Some functions return an OperationHandle. In those cases, the Client should periodically call
   * getProgress() to see how it's coming. When OperationProgress returns with complete set to true,
   * the Client must call acknowledgeComplete() to say that it knows everything has finished properly.
   */
  def getProgress(handle:OperationHandle):Future[OperationProgress]
  
  def acknowledgeComplete(handle:OperationHandle):Unit
}

/**
 * The handle that API entry points can return for long-running operations. The client must then
 * call CommonFunctions.checkProgress() using that handle periodically.
 */
sealed trait OperationHandle
case class ActorOperationHandle(path:String) extends OperationHandle

/**
 * A description of the current state of a long-running operation.
 */
case class OperationProgress(msg:String, percent:Int, complete:Boolean, failed:Boolean)

trait PassthroughHandlerBase {
  def pass(name:String):ThingInfo
}

/**
 * This is tricky magic. PassthroughHandler is defined completely differently on the Server
 * and Client, so that the Server mashalls and the Client unmarshalls from the same calls.
 *
 * IMPORTANT: this uses Names to find these values! This is more fragile than I like, but is
 * the tradeoff I'm making for the time being.
 */
class StandardThings(h:PassthroughHandlerBase) {
  object core {
    val exactlyOneColl = h.pass("Exactly One")
  	val optionalColl = h.pass("Optional")
  	val listColl = h.pass("List")
  	val setColl = h.pass("Set")
  
    val urProp = h.pass("Property")
    val nameProp = h.pass("Name")
    val collectionProp = h.pass("Property Collection")
    val typeProp = h.pass("Property Type")
    val isModelProp = h.pass("Is a Model")
  }
  
  object basic {
    val simpleThing = h.pass("Simple-Thing")
    val displayNameProp = h.pass("Display Name")
  	val defaultView = h.pass("Default View")
  	val printView = h.pass("Print View")
  }
  
  object conventions {
    val detailsProp = h.pass("Details")
    val summaryProp = h.pass("Summary")  
  }
  
  object editing {
    val instancePropsProp = h.pass("Instance Properties")
  }
  
  object security {
    val inviteTextProp = h.pass("Space Invitation Text")
    val personRolesProp = h.pass("Person Roles")
  }
  
  object tags {
    val isReifiedTag = h.pass("_Is Reified Tag")
  }
  
  object types {
    val deriveNameProp = h.pass("_deriveName")
  	val deriveAlways = h.pass("Always Derive Name")
  	val deriveNever = h.pass("Never Derive Name")
  }
  
  // This is necessary in order to force the objects to come into being. Each of the
  // above objects must be named here:
  def touchEverything() = {
    Seq(core, basic, conventions, editing, security, tags, types)
  }
}
