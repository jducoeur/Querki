package querki.session

import scala.concurrent.{Future, Promise}

import akka.actor._

import models.{DisplayText, Thing, ThingId, Wikitext}

import querki.globals._

import querki.api.ThingFunctions
import querki.core.QLText
import querki.data.{PropValInfo, RequestInfo, ThingInfo}
import querki.pages.ThingPageDetails
import querki.spaces.messages.{DeleteThing, ThingFound, ThingError}
import querki.util.Requester

trait ThingFunctionsImpl extends SessionApiImpl with ThingFunctions { self:Actor with Requester =>
  
  def ClientApi:querki.api.ClientApi
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  def QL:querki.ql.QL
  
  def getRequestInfo():RequestInfo = ClientApi.requestInfo(rc)
  
  def getThingInfo(thingId:String) = withThing(thingId) { thing =>
    ClientApi.thingInfo(thing, rc)
  }

  def getThingPage(thingId:String):ThingPageDetails = withThing(thingId) { thing =>
    implicit val state = rc.state.get
    
    val thingInfo = ClientApi.thingInfo(thing, rc)
    // Note that both the root Thing and (more importantly) TagThings won't have a Model:
    val modelInfo = for {
      model <- thing.getModelOpt
    } 
      yield ClientApi.thingInfo(model, rc)
    
    val customHeaderOpt = for {
      pv <- thing.getPropOpt(HtmlUI.PageHeaderProperty)
    }
      yield pv.v.wikify(thing.thisAsContext(rc))

    val rendered = thing.render(rc)
    
    ThingPageDetails(thingInfo, modelInfo, customHeaderOpt, rendered)
  }
  
  def evaluateQL(thingId:String, ql:String):Wikitext = withThing(thingId) { thing =>
    implicit val r = rc
    val context = thing.thisAsContext
    QL.processMethod(QLText(ql), context, None, Some(thing)).wikify(context)
  }
  
  def getProperties(thingId:String):Seq[PropValInfo] = withThing(thingId) { thing =>
    ClientApi.propValInfo(thing, rc)
  }
  
  /**
   * TODO: rewrite this using the new Requester Monad, after I write that. Use it to help
   * drive the question of how we propagate an exception inside the ThingError.
   */
  def deleteThing(thingId:String):Future[Unit] = withThing(thingId) { thing =>
    val promise = Promise[Unit]
    
    spaceRouter.request(DeleteThing(user, state.owner, state.toThingId, thing.toThingId)) {
      // TODO: there is no longer an obvious reason to return newState here, and probably good
      // reasons not to:
      case ThingFound(thingId, newState) => promise.success(())
      // TODO: we don't need stateOpt here any more:
      case ThingError(error, stateOpt) => promise.failure(error)
    }
    
    promise.future
  }
  
  def getNumInstances(modelId:String):Int = withThing(modelId) { model =>
    // TODO: once we are caching the hierarchy tree, this can become more efficient:
    state.descendants(model.id, false, true).size
  }
}
