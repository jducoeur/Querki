package querki.test.mid

import autowire._

import models.Wikitext
import querki.api._
import querki.data._
import querki.globals._
import querki.pages.ThingPageDetails

import ThingFunctions._
import AllFuncs._


trait ThingFuncs {

  /////////////////////////////
  //
  // Wrappers for the ThingFunctions API functions.
  //

  def getRequestInfo(): TestOp[RequestInfo] =
    TestOp.client { _[ThingFunctions].getRequestInfo().call() }

  def getThingPage(thingId:TID, renderPropId:Option[TID]): TestOp[ThingPageDetails] =
    TestOp.client { _[ThingFunctions].getThingPage(thingId, renderPropId).call() }

  def getThingInfo(thingId: TID): TestOp[ThingInfo] =
    TestOp.client { _[ThingFunctions].getThingInfo(thingId).call() }

  def evaluateQL(thingId:TID, ql:String): TestOp[Wikitext] =
    TestOp.client { _[ThingFunctions].evaluateQL(thingId, ql).call() }

  def evaluateQLWithContext(typeId:TID, context:String, lexical:Option[TID], ql:String): TestOp[Wikitext] =
    TestOp.client { _[ThingFunctions].evaluateQLWithContext(typeId, context, lexical, ql).call() }

  def getProperties(thingId:TID): TestOp[Seq[PropValInfo]] =
    TestOp.client { _[ThingFunctions].getProperties(thingId).call() }

  def getPropertyValues(thingId: TID, props: List[TOID]): TestOp[Map[TOID, PV]] =
    TestOp.client { _[ThingFunctions].getPropertyValues(thingId, props).call() }

  def getPropertyDisplay(thingId:TID, propId:TID): TestOp[Option[Wikitext]] =
    TestOp.client { _[ThingFunctions].getPropertyDisplay(thingId, propId).call() }

  def getAllProperties(): TestOp[SpaceProps] =
    TestOp.client { _[ThingFunctions].getAllProperties().call() }

  def getAllTypes(): TestOp[AllTypeInfo] =
    TestOp.client { _[ThingFunctions].getAllTypes().call() }

  def deleteThing(thingId:TID): TestOp[Unit] =
    TestOp.client { _[ThingFunctions].deleteThing(thingId).call() }

  def getNumInstances(modelId:TID): TestOp[Int] =
    TestOp.client { _[ThingFunctions].getNumInstances(modelId).call() }

  def getChildren(modelId:TID, includeModels:Boolean, includeInstances:Boolean): TestOp[Seq[ThingInfo]] =
    TestOp.client { _[ThingFunctions].getChildren(modelId, includeModels, includeInstances).call() }

  def reloadSpace(): TestOp[Unit] =
    TestOp.client { _[ThingFunctions].reloadSpace().call() }

  /////////////////////////////
}
