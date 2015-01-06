package querki.editing

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import org.scalajs.jqueryui._
import scalatags.JsDom.all._
import autowire._
import rx._
import rx.ops._

import querki.globals._

import querki.api.{EditFunctions, StandardThings}
import EditFunctions._
import querki.data.{BasicThingInfo, PropInfo, SpaceProps, ThingInfo}
import querki.display.{ButtonGadget, ButtonKind, Gadget, RawDiv, WithTooltip}
import querki.display.input.{DeleteInstanceButton, InputGadget}
import querki.display.rx.{RxDiv, RxThingSelector}
import querki.pages._

class ModelDesignerPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {
  
  lazy val modelId = TID(params.get("modelId").getOrElse(params("thingId")))
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataModel = interface[querki.datamodel.DataModel]
  lazy val Editing = interface[Editing]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  // We start loading this at page load, so that it's available when we inspect Properties.
  val allTypesFut = DataAccess.getAllTypes()
  // Note that we pro-actively begin loading this immediately. It's one of the more common operations for the
  // Model Designer, and we want quick response.
  val allPropsFut = DataAccess.getAllProps()
  
  /**
   * This is a Future of a Map of all of the known Properties, by OID.
   */
  val propMapFut = allPropsFut.map { mainSpaceProps =>
    def spacePropsRec(spaceProps:SpaceProps):Seq[PropInfo] = {
      spaceProps.standardProps ++ spaceProps.advancedProps ++ spaceProps.apps.flatMap(spacePropsRec(_))
    }
    
    val allProps = spacePropsRec(mainSpaceProps)
    Map(allProps.map(propInfo => (propInfo.oid -> propInfo)):_*)
  }
  
  /**
   * This is a Future of a Map of all the known Types, by OID.
   */
  val typeMapFut = allTypesFut.map { allTypeInfo =>
    val allTypes = allTypeInfo.standardTypes ++ allTypeInfo.advancedTypes
    Map(allTypes.map(typeInfo => (typeInfo.oid -> typeInfo)):_*)
  }
  
  /**
   * This is a Future of a Map of all the known Collections, by OID.
   */
  val collMapFut = allTypesFut.map { allTypeInfo =>
    Map(allTypeInfo.collections.map(collInfo => (collInfo.oid -> collInfo)):_*)
  }
  
  /**
   * This is a Future of a Map of all the usable Models, by OID.
   */
  val modelMapFut = allTypesFut.map { allTypeInfo =>
    Map(allTypeInfo.models.map(modelInfo => (modelInfo.oid -> modelInfo)):_*)
  }
  
  override def beforeRender() = {
    // Page-specific gadget hooks:
    // TODO: these need to be updated for the new Bootstrap. Can we come up with a better abstraction here?
    Gadgets.registerHook("._largeTextEdit") { elem => $(elem).addClass("span10") }
    Gadgets.registerHook("input[type='text']") { elem => $(elem).filter(".propEditor").addClass("span10") }    
  }
  
  def addProperty(propId:TID, openEditor:Boolean = false) = {
    Client[EditFunctions].addPropertyAndGetEditor(modelId, propId).call().foreach { editInfo =>
      // TODO: introduce the concept of Properties that are mainly for Models; if that is
      // set, put it in the Model section instead:
      instancePropSection().appendEditor(editInfo, openEditor)
      InputGadgets.hookPendingGadgets()
    }
  }
  
  def removeProperty(editor:PropValueEditor) = {
    Client[EditFunctions].removeProperty(modelId, editor.propInfo.oid).call().foreach { result =>
      result match {
        case PropertyChanged => editor.section.removeEditor(editor)
        case PropertyChangeError(msg) => StatusLine.showBriefly(msg)        
      }
    }
  }
  
  def page = this
  
  class PropSectionHolder {
    var _propSection:Option[PropertySection] = None
    def make(thing:BasicThingInfo, sortedProps:Seq[PropEditInfo], editInfo:FullEditInfo, stdThings:StandardThings, path:String) = {
      _propSection = 
        Some(new PropertySection(page, path, sortedProps, thing, editInfo, stdThings,
          thing match {
            case t:ThingInfo => t.isModel
            case _ => false
          }))
      _propSection
    }
    def apply() = _propSection.get    
    def exists = _propSection.isDefined
  }
  val instancePropSection = new PropSectionHolder
  val modelPropSection = new PropSectionHolder

  def pageContent = {
    for {
      model <- DataAccess.getThing(modelId)
      modelModel <- DataAccess.getThing(model.modelOid)
      fullEditInfo <- Client[EditFunctions].getPropertyEditors(modelId).call()
      stdThings <- DataAccess.standardThings
      allProps = fullEditInfo.propInfos
      (instanceProps, modelProps) = allProps.partition(propEditInfo => fullEditInfo.instancePropIds.contains(propEditInfo.propInfo.oid))
      sortedInstanceProps = (Seq.empty[PropEditInfo] /: fullEditInfo.instancePropIds) { (current, propId) =>
        instanceProps.find(_.propInfo.oid == propId) match {
          case Some(prop) => current :+ prop
          case None => { println(s"Couldn't find property $propId, although it is in instancePropIds!"); current }
        }
      }
      pageTitle = {
	    if (model.isModel)
	      s"Designing Model ${model.displayName}"
	    else
	      s"Editing ${model.displayName}"
      }
	  guts = 
        div(cls:="_advancedEditor",
          h1(pageTitle),
          p(cls:="_smallSubtitle", 
            s"Model: ${modelModel.displayName} -- ",
            a("Change Model", 
              href:=PageManager.currentHash,
              onclick:={ () => 
                DataModel.changeModel(
                  model,
                  { newThingInfo => PageManager.reload() }) 
            })
          ),
          if (model.isModel) {
            MSeq(
              h3(cls:="_defaultTitle", "Instance Properties"),
              p(cls:="_smallSubtitle", 
                """These are the Properties that can be different for each Instance. Drag a Property into here if you
                  |want to edit it for each Instance, or out if you don't. The order of the Properties here will be
                  |the order they show up in the Instance Editor.""".stripMargin),
              instancePropSection.make(model, sortedInstanceProps, fullEditInfo, stdThings, fullEditInfo.instancePropPath),
              new AddPropertyGadget(this, model),
              h3(cls:="_defaultTitle", "Model Properties"),
              p(cls:="_smallSubtitle", "These Properties are the same for all Instances of this Model"),
              modelPropSection.make(model, modelProps, fullEditInfo, stdThings, "modelProps")
            )
          } else {
            MSeq(
              instancePropSection.make(model, allProps, fullEditInfo, stdThings, "allProps"),
              new AddPropertyGadget(this, model)
            )
          },
          a(cls:="btn btn-primary",
            "Done",
            href:=thingUrl(model))
        )
    }
      yield PageContents(pageTitle, guts)
  }
}
