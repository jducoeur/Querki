package querki.pages

import org.querki.jquery._
import scalatags.JsDom.all.{input => inp, _}
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.data.ThingInfo
import querki.display.QText
import querki.display.input.InputGadget
import querki.editing.EditFunctions
import EditFunctions.{ChangePropertyValue, PropertyChange}

object EditQL {
  def apply() = """""[[_edit]]
                    |
                    |[[_oidLink -> _mixedButton(""share-alt"", ""Done"")]]
                    |
                    |""""".stripMargin
}

class CreateAndEditPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember  {

  lazy val Client = interface[querki.client.Client]
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val Editing = interface[querki.editing.Editing]
  
  lazy val modelId = TID(params("model"))
  
  lazy val _reifyTag = params.get("reifyTag")
  def reifyTag = _reifyTag.isDefined
  
  var thingInfoOpt:Option[ThingInfo] = None

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
    yield PageContents(s"Create a ${modelInfo.displayName}", guts)
    
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
    findGadgets(control).foreach {
      case gadget:InputGadget[_] => {
        gadget.setValue(v)
      }
    }
  }
}
