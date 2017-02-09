package querki.editing

import scala.scalajs.js
import org.scalajs.dom
import org.querki.jquery._
import org.querki.facades.jqueryui._
import scalatags.JsDom.all._
import autowire._
import rx._
import rx.ops._

import querki.globals._

import EditFunctions._
import querki.api.{ModelLoopException, ThingFunctions}
import querki.data.{BasicThingInfo, PropInfo, SpaceProps, ThingInfo}
import querki.display.{RawDiv, WithTooltip}
import querki.display.input.{InputGadget}
import querki.display.rx.{RxThingSelector}
import querki.pages._

class ModelDesignerPage(params:ParamMap)(implicit val ecology:Ecology) 
  extends Page("modelDesigner") with querki.display.QuerkiUIUtils
{
  
  lazy val rawModelIdOpt = params.get("modelId").orElse(params.get("thingId"))
  var _modelId:Option[TID] = None
  def modelId = _modelId.get
  
  override def beforeRender() = {
    // Page-specific gadget hooks:
    Gadgets.registerHook("._largeTextEdit") { elem => $(elem).addClass("col-md-10 form-control") }
    Gadgets.registerHook("input[type='text']") { elem => 
      val inputs = $(elem).filter(".propEditor").filter(":not(._tagSetInput)")
      inputs.addClass("col-md-10 form-control") 
    }
    
    // If a Model hasn't been specified, pop a dialog:
    rawModelIdOpt match {
      case Some(mid) => {
        _modelId = rawModelIdOpt.map(TID(_))
        Future.successful(())
      }
      case _ => {
        DataModel.chooseAModel(
          "Design a Model", 
          "Choose which Model to base the new one on (just use Simple Thing if not sure)", 
          "Create Model"
        ).flatMap { selection =>
          if (selection.isEmpty)
            Future.successful(PageManager.showRoot())
          else {
            val initProps = 
              Seq(
                ChangePropertyValue(Editing.propPath(std.core.isModelProp), Seq("true"))
              )
            Client[EditFunctions].create(selection.get, initProps).call().map { modelInfo =>
               _modelId = Some(modelInfo.oid)
            }
          }
        }
      }
    }
  }
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataModel = interface[querki.datamodel.DataModel]
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val Editing = interface[Editing]
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
  
  def addProperty(propId:TID, openEditor:Boolean = false) = {
    Client[EditFunctions].addPropertyAndGetEditor(modelId, propId).call().foreach { editInfo =>
      // TODO: introduce the concept of Properties that are mainly for Models; if that is
      // set, put it in the Model section instead:
      instancePropSection().appendEditor(editInfo, openEditor)
      Gadgets.hookPendingGadgets()
    }
  }
  
  def removeProperty(editor:PropValueEditor) = {
    val propToRemove = editor.propInfo.oid
    // TODO: this should handle exceptions!
    Client[EditFunctions].removeProperty(modelId, propToRemove).call().foreach { result =>
      result match {
        case PropertyChanged => {
          // Remove the Editor, and update Instance Props if needed:
          editor.section.removeEditor(editor)
          // If the Property is local...
          if (!editor.propInfo.isShadow) {
            Client[EditFunctions].getPropertyUsage(propToRemove).call() foreach { usage =>
              // ... and it's not being used any more..
              if (usage.nModels == 0 && usage.nInstances == 0) {
                // ... then delete it completely:
                Client[ThingFunctions].deleteThing(propToRemove).call().foreach { dummy =>
                  // Do we need to do anything here?
                }
              }
            }
          }
        }
      }
    }
  }
  
  def page = this
  
  class PropSectionHolder {
    var _propSection:Option[PropertySection] = None
    def make(thing:BasicThingInfo, sortedProps:Seq[PropEditInfo], editInfo:FullEditInfo, path:String) = {
      _propSection = 
        Some(new PropertySection(page, path, sortedProps, thing, editInfo,
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
  
  def checkParams:Option[Future[PageContents]] = {
    println(s"The modelId is '${modelId.underlying}' -- isEmpty is ${modelId.isEmpty}")
    if (modelId.isEmpty) {
      val guts =
        div(p("The URL of this page is missing its modelId. This may indicate a Querki bug; please report it. Sorry about that."))
      Some(Future.successful(PageContents("ModelId missing!", guts)))
    } else
      None
  }

  def pageContent = {
    checkParams getOrElse {
      for {
        model <- DataAccess.getThing(modelId)
        _ = DataSetting.setThing(Some(model))
        modelModel <- DataAccess.getThing(model.modelOid)
        fullEditInfo <- 
          Client[EditFunctions].getPropertyEditors(modelId).call()
            .recover {
              case ModelLoopException() => {
                StatusLine.showUntilChange("You appear to have a Model Loop. Please change Models to fix this.")
                FullEditInfo(Seq.empty, "", true, Seq.empty)
              }
            }
        allProps = fullEditInfo.propInfos
        (instanceProps, modelProps) = allProps.partition(propEditInfo => fullEditInfo.instancePropIds.contains(propEditInfo.propInfo.oid))
        sortedInstanceProps = (Seq.empty[PropEditInfo] /: fullEditInfo.instancePropIds) { (current, propId) =>
          instanceProps.find(_.propInfo.oid == propId) match {
            case Some(prop) => current :+ prop
            case None => { println(s"Couldn't find property $propId, although it is in instancePropIds!"); current }
          }
        }
        pageTitle = {
          val prefix = 
            if (model.isModel)
              msg("modelPrefix")
            else
              msg("thingPrefix")
          msg("pageTitle", ("modelName" -> model.unsafeName), ("prefix" -> prefix))
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
                instancePropSection.make(model, sortedInstanceProps, fullEditInfo, fullEditInfo.instancePropPath),
                new AddPropertyGadget(this, model),
                h3(cls:="_defaultTitle", "Model Properties"),
                p(cls:="_smallSubtitle", "These Properties are the same for all Instances of this Model"),
                modelPropSection.make(model, modelProps, fullEditInfo, "modelProps")
              )
            } else {
              MSeq(
                instancePropSection.make(model, allProps, fullEditInfo, "allProps"),
                new AddPropertyGadget(this, model)
              )
            },
            a(cls:="btn btn-primary",
              id:="_doneDesigning",
              "Done",
              href:=thingUrl(model.oid))
          )
      }
        yield PageContents(pageTitle, guts)
    }
  }
}
