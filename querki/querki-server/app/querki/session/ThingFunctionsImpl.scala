package querki.session

import scala.concurrent.{Future, Promise}

import akka.actor._

import models.{Collection, DisplayText, Kind, PType, Thing, ThingId, Wikitext}

import querki.globals._

import querki.api.ThingFunctions
import querki.core.QLText
import querki.data._
import querki.pages.ThingPageDetails
import querki.spaces.messages.{DeleteThing, ThingFound, ThingError}
import querki.util.Requester

class ThingFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with ThingFunctions {
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Core = interface[querki.core.Core]
  lazy val Editor = interface[querki.editing.Editor]
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val QL = interface[querki.ql.QL]
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]
  lazy val Stylesheets = interface[querki.css.Stylesheets]
  
  def getRequestInfo():RequestInfo = ClientApi.requestInfo(rc)
  
  def getThingInfo(thingId:TID) = withThing(thingId) { thing =>
    ClientApi.thingInfo(thing, rc)
  }

  def getThingPage(thingId:TID):ThingPageDetails = withThing(thingId) { thing =>
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
    
    val styleinfo = Stylesheets.stylesheetsFor(thing)
    
    ThingPageDetails(thingInfo, modelInfo, customHeaderOpt, rendered, styleinfo.sheets, styleinfo.headers)
  }
  
  def evaluateQL(thingId:TID, ql:String):Wikitext = withThing(thingId) { thing =>
    implicit val r = rc
    val context = thing.thisAsContext
    QL.processMethod(QLText(ql), context, None, Some(thing)).wikify(context)
  }
  
  def getProperties(thingId:TID):Seq[PropValInfo] = withThing(thingId) { thing =>
    ClientApi.propValInfo(thing, rc)
  }
  
  def getPropertyDisplay(thingId:TID, propIdStr:TID):Option[Wikitext] = withThing(thingId) { thing =>
    implicit val s = state
    implicit val r = rc
    val propId = propIdStr.toThingId
    for {
      prop <- state.prop(propId)
      pv <- thing.getPropOpt(prop)
      if (!pv.isEmpty)
    }
      yield pv.render(thing.thisAsContext.forProperty(pv.prop), Some(thing))
  }
  
  def getAllProperties():SpaceProps = {
    // We dive recursively up the app tree to construct the SpaceProps:
    def getPropsForSpace(space:SpaceState):SpaceProps = {
      implicit val s = space
      
      // We want a given Prop iff it is the right Kind, and is neither Internal nor SystemOnly:
      def filterProps(props:Seq[AnyProp]):Seq[PropInfo] = {
        val filtered = for {
          prop <- props
          if (!prop.ifSet(Core.InternalProp))
          if (!prop.ifSet(Basic.SystemOnlyProp))
        }
          yield prop
          
        filtered.map(prop => ClientApi.propInfo(prop, rc))
      }
      
      SpaceProps(
        space,
        space.linkName,
        space.displayName,
        filterProps(SkillLevel.standardProps),
        filterProps(SkillLevel.advancedProps),
        space.app.map(app => Seq(getPropsForSpace(app))).getOrElse(Seq.empty)
      )
    }
    
    getPropsForSpace(state)
  }
  
  def getAllTypes():AllTypeInfo = {
    implicit val s = state
      
    val spaceTypes = state.allTypes.values.
        filterNot(_.ifSet(Core.InternalProp)).
        filterNot(_.ifSet(Basic.DeprecatedProp)).
        filterNot(typ => typ.isInstanceOf[querki.types.ModelTypeBase] && !typ.ifSet(Basic.ExplicitProp))
    
    def toCollectionInfo(coll:Collection) = CollectionInfo(coll, coll.linkName, coll.displayName)
    
    // TODO: separate the Types by SkillLevel:
    AllTypeInfo(
      Seq(Core.ExactlyOne, Core.Optional, Core.QList, Core.QSet).map(toCollectionInfo(_)),
      Seq.empty,
      spaceTypes.map(typ => TypeInfo(typ, typ.linkName, typ.displayName)).toSeq,
      state.allModels.filter(_.hasProp(Editor.InstanceProps)(state)).map(ClientApi.thingInfo(_, rc)).toSeq
    )
  }
  
  /**
   * TODO: rewrite this using the new Requester Monad, after I write that. Use it to help
   * drive the question of how we propagate an exception inside the ThingError.
   */
  def deleteThing(thingId:TID):Future[Unit] = withThing(thingId) { thing =>
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
  
  def getNumInstances(modelId:TID):Int = withThing(modelId) { model =>
    // TODO: once we are caching the hierarchy tree, this can become more efficient:
    state.descendants(model.id, false, true).size
  }
}
