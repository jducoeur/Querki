package querki.pages

import org.querki.jquery._
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.{GadgetLookup, QText}
import querki.display.input.InputGadget
import querki.editing.EditFunctions
import EditFunctions.{ChangePropertyValue, PropertyChange}

object EditQL {
  def apply() = """""[[_edit]]
                    |
                    |[[_oidLink -> _linkButton(icon=""share-alt"", label=""Done"", id=""_editDoneButton"")]]
                    |
                    |""""".stripMargin
}

class CreateAndEditPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("createAndEdit") {

  lazy val Client = interface[querki.client.Client]
  lazy val DataModel = interface[querki.datamodel.DataModel]
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val Editing = interface[querki.editing.Editing]
  
  lazy val rawModelIdOpt = params.get("model")
  var _modelId:Option[TID] = None
  def modelId = _modelId.get
  
  lazy val _reifyTag = params.get("reifyTag")
  def reifyTag = _reifyTag.isDefined
  
  var thingInfoOpt:Option[ThingInfo] = None
  
  override def beforeRender() = {
    rawModelIdOpt match {
      case Some(mid) => {
        _modelId = rawModelIdOpt.map(TID(_))
        Future.successful(())
      }
      case _ => {
        DataModel.chooseAModel(
          "Create a Thing", 
          "What kind of Thing do you want to create? (Just use Simple Thing if you just want a plain page.)", 
          "Create"
        ).map { selection =>
          if (selection.isEmpty)
            PageManager.showRoot()
          else
            _modelId = selection
        }
      }
    }
  }

  def pageContent = for {
    modelInfo <- Client[ThingFunctions].getThingInfo(modelId).call()
    initVals <- initialValues
    thingInfo <- Client[EditFunctions].create(modelId, initVals).call()
    dummy = {
      DataSetting.setThing(Some(thingInfo))
      DataSetting.setModel(Some(modelInfo))
      thingInfoOpt = Some(thingInfo)
    }
    editor <- Client[ThingFunctions].evaluateQL(thingInfo.oid, EditQL()).call()
    guts = div(new QText(editor))
  }
    yield PageContents(msg("pageTitle", ("modelName" -> modelInfo.unsafeName)), guts)
    
  def initialValues:Future[Seq[PropertyChange]] = {
    val otherParams = params - "model" - "reifyTag"
    val changes = otherParams.map { pair =>
      val (path, v) = pair
      ChangePropertyValue(path, List(v))
    }.toSeq
    
    // If we're reifying a Tag, say that we're doing so:
    val withReify = 
      if (reifyTag) {
        changes :+ ChangePropertyValue(Editing.propPath(std.tags.isReifiedTag), List("true"))
      } else {
        changes
      }
    Future.successful(withReify)
  }
    
  /**
   * First pass at allowing other systems to inject values into a newly-created page.
   * 
   * TODO: this is very crude in many ways, but I'm not sure yet what the right approach is.
   * It is mainly intended as a way to create a page and set a Large Text value, which we don't
   * really want to do through the URL. Conceptually, it's like an internal POST form submission.
   * It is currently used from the Explore page.
   */
  def setValue(prop:TID, v:String) = {
    // Controls are identified by their path. Assuming the desired Property is here, its path
    // should exist as an ID:
    val path = Editing.propPathOldStyleHack(prop, thingInfoOpt.map(_.oid))
    val control = $(elem).find(s"#$path")
    GadgetLookup.findGadgets(control).foreach {
      case gadget:InputGadget[_] => {
        gadget.setValue(v)
      }
    }
  }
}
