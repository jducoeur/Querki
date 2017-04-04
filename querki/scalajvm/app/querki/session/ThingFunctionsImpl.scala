package querki.session

import scala.concurrent.Promise

import akka.actor._

import models.{Collection, DisplayText, Kind, PType, Thing, ThingId, Wikitext}

import querki.globals._

import querki.api.{SpaceApiImpl, AutowireParams, ThingFunctions}
import querki.core.QLText
import querki.data._
import querki.pages.ThingPageDetails
import querki.spaces.messages.{DeleteThing, ThingFound, ThingError}

class ThingFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with ThingFunctions {
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Core = interface[querki.core.Core]
  lazy val Editor = interface[querki.editing.Editor]
  lazy val HtmlUI = interface[querki.html.HtmlUI]
  lazy val Profiler = interface[querki.tools.Profiler]
  lazy val QL = interface[querki.ql.QL]
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]
  lazy val Stylesheets = interface[querki.css.Stylesheets]
  
  def doRoute(req:Request):Future[String] = route[ThingFunctions](this)(req)
  
  def getRequestInfo():Future[RequestInfo] = ClientApi.requestInfo(rc)(state)
  
  def getThingInfo(thingId:TID):Future[ThingInfo] = withThing(thingId) { thing =>
    ClientApi.thingInfo(thing, rc)(state)
  }

  lazy val getThingPageProfiler = Profiler.createHandle("Session.getThingPage")
  lazy val pageRenderProfiler = Profiler.createHandle("Session.renderThingPage")
  
  def getThingPage(thingId:TID, renderPropIdOpt:Option[TID]):Future[ThingPageDetails] = getThingPageProfiler.profileFut { withThing(thingId) { thing =>
    implicit val s = state
    
    // Note that both the root Thing and (more importantly) TagThings won't have a Model:
    val modelInfoOptFut = for {
      model <- thing.getModelOpt
    } 
      yield ClientApi.thingInfo(model, rc)
    
    val customHeaderPropOpt = for {
      pv <- thing.getPropOpt(HtmlUI.PageHeaderProperty)
      if (!thing.isModel)
    }
      yield pv.v.wikify(thing.thisAsContext(rc, state, ecology))
      
    val renderPropOpt = renderPropIdOpt.flatMap { propTid =>
      val oid = ThingId(propTid.underlying)
      // Either show this actual Thing, or a synthetic TagThing if it's not found:
      state.prop(oid)
    }

    val styleinfo = Stylesheets.stylesheetsFor(thing)
    
    for {
      rendered <- pageRenderProfiler.profileFut { thing.render(rc, state, renderPropOpt) }
      thingInfo <- ClientApi.thingInfo(thing, rc)
      modelInfo <- modelInfoOptFut.map(_.map(Some(_))).getOrElse(Future.successful(None))
      customHeaderOpt <- futOpt(customHeaderPropOpt)
    }
      yield ThingPageDetails(thingInfo, modelInfo, customHeaderOpt, rendered, styleinfo.sheets, styleinfo.headers)
  }}
  
  def evaluateQL(thingId:TID, ql:String):Future[Wikitext] = withThing(thingId) { thing =>
    implicit val r = rc
    implicit val s = state
    val context = thing.thisAsContext(rc, state, ecology)
    QL.processMethodToWikitext(QLText(ql), context, None, Some(thing))
  }
  
  def evaluateQLWithContext(typeId:TID, serializedContext:String, lexicalOpt:Option[TID], ql:String):Future[Wikitext] = withThing(typeId) { case pt:PType[_] =>
    implicit val r = rc
    implicit val s = state
    val qv = Core.ExactlyOne(pt.deserialize(serializedContext))
    val context = querki.values.QLContext(qv, Some(rc))
    val lexicalContext = lexicalOpt.flatMap { lex =>
      val oid = ThingId(lex.underlying)
      state.anything(oid)
    }
    QL.processMethodToWikitext(QLText(ql), context, None, lexicalContext)
  }
  
  def getProperties(thingId:TID):Future[Seq[PropValInfo]] = withThing(thingId) { thing =>
    ClientApi.propValInfo(thing, rc)(state)
  }
  
  def getPropertyDisplay(thingId:TID, propIdStr:TID):Future[Option[Wikitext]] = withThing(thingId) { thing =>
    implicit val s = state
    implicit val r = rc
    val propId = propIdStr.toThingId
    futOpt(for {
      prop <- state.prop(propId)
      pv <- thing.getPropOpt(prop)
      if (!pv.isEmpty)
    }
      yield pv.render(thing.thisAsContext(rc, state, ecology).forProperty(pv.prop), Some(thing)))
  }
  
  def getAllProperties():SpaceProps = {
    // We dive recursively up the app tree to construct the SpaceProps
    // TODO: this is irritatingly hard-coded, and redundant with the general functions in SystemState.
    // Can we make this less dependent on that?
    def getPropsForSpace(space:SpaceState, withSystem:Boolean):SpaceProps = {
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
        space.apps.map(getPropsForSpace(_, false)) ++ (if (withSystem) Seq(getPropsForSpace(space.system.get, false)) else Seq.empty)
      )
    }
    
    getPropsForSpace(state, true)
  }
  
  def getAllTypes():Future[AllTypeInfo] = {
    implicit val s = state
      
    val spaceTypes = state.allTypes.values.
        filterNot(_.ifSet(Core.InternalProp)).
        filterNot(_.ifSet(Basic.DeprecatedProp)).
        filterNot(typ => typ.isInstanceOf[querki.types.ModelTypeBase] && !typ.ifSet(Basic.ExplicitProp))
    
    def toCollectionInfo(coll:Collection) = CollectionInfo(coll, coll.linkName, coll.displayName)
    
    // TODO: separate the Types by SkillLevel:
    Future.sequence(state.allModels.filter(_.hasProp(Editor.InstanceProps)(state)).map(ClientApi.thingInfo(_, rc)).toSeq) map { modelInfo =>
      AllTypeInfo(
        Seq(Core.ExactlyOne, Core.Optional, Core.QList, Core.QSet).map(toCollectionInfo(_)),
        Seq.empty,
        spaceTypes.map(typ => TypeInfo(typ, typ.linkName, typ.displayName, typ.firstOpt(Editor.PreferredCollectionProp).map(_.toTID))).toSeq,
        modelInfo
      )
    }
  }
  
  def deleteThing(thingId:TID):Future[Unit] = withThing(thingId) { thing =>
    spaceRouter.request(DeleteThing(user, state.id, thing.toThingId)) map {
      // TODO: there is no longer an obvious reason to return newState here, and probably good
      // reasons not to:
      case ThingFound(thingId, newState) => ()
      // TODO: we don't need stateOpt here any more:
      case ThingError(error, stateOpt) => throw error
    }
  }
  
  def getNumInstances(modelId:TID):Int = withThing(modelId) { model =>
    // TODO: once we are caching the hierarchy tree, this can become more efficient:
    state.descendants(model.id, false, true).size
  }
  
  def getChildren(modelId:TID, includeModels:Boolean, includeInstances:Boolean):Future[Seq[ThingInfo]] = withThing(modelId) { model =>
    implicit val s = state
    val result = state.children(model) filter { t =>
      if (t.isModel)
        includeModels
      else
        includeInstances
    } map (ClientApi.thingInfo(_,rc))
    Future.sequence(result.toSeq)
  }
  
  def reloadSpace():Future[Unit] = {
    Future.successful(spaceRouter ! querki.util.Reload)
  }
}
