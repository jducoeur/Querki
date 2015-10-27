package querki.api

import scala.concurrent.Future

import models.Kind

import querki.data.{ThingInfo, TID}

class CommonFunctionsEmpty extends CommonFunctions {
  // Note that we aren't even trying to use the actual OIDs for the standard Things. That's okay, since
  // TIDs are opaque to the Client anyway.
  def toTID(name:String) = TID(s".${name.replace(" ", "-").toLowerCase}")
  def tiBase(name:String, modelName:String, kind:Kind.Kind) = ThingInfo(toTID(name), Some(name), name, toTID(modelName), kind, false, false, false, false, false, None)
  def ti(name:String) = tiBase(name, "Thing", Kind.Thing)
  def tip(name:String) = tiBase(name, "Property", Kind.Property)
  
  def tpair(name:String) = name -> ti(name)
  def ppair(name:String) = name -> tip(name)
  
  def getStandardThings():Future[Map[String, ThingInfo]] = Future.successful(Map(
    tpair("Property"),
    ppair("Name"),
    ppair("Property Collection"),
    ppair("Property Type"),
    ppair("Is a Model"),
    
    tpair("Simple-Thing"),
    ppair("Display Name"),
    ppair("Default View"),
    ppair("Print View"),
    
    ppair("Details"),
    ppair("Summary"),
    
    ppair("Instance Properties"),
    
    ppair("Space Invitation Text"),
    ppair("Person Roles"),
    
    ppair("_deriveName"),
    tpair("Always Derive Name"),
    tpair("Never Derive Name")
  ))
  
  def getProgress(handle:OperationHandle):Future[OperationProgress] = ???  
  def acknowledgeComplete(handle:OperationHandle):Unit = ???
}
