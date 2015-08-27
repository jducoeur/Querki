package querki.api

import scala.concurrent.Future

import models.Wikitext

import querki.data._
import querki.pages.ThingPageDetails

class ThingFunctionsEmpty extends ThingFunctions {
  def getRequestInfo():RequestInfo = ???
  def getThingPage(thingId:TID, propIdOpt:Option[TID], params:Map[String,String]):ThingPageDetails = ???
  def getThingInfo(thingId:TID):ThingInfo = ???
  def evaluateQL(thingId:TID, ql:String):Wikitext = ???
  def getProperties(thingId:TID):Seq[PropValInfo] = ???
  def getPropertyDisplay(thingId:TID, propId:TID):Option[Wikitext] = ???
  def getAllProperties():SpaceProps = ???
  def getAllTypes():AllTypeInfo = ???
  def deleteThing(thingId:TID):Future[Unit] = ???
  def getNumInstances(modelId:TID):Int = ???
}
