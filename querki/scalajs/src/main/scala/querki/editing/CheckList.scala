package querki.editing

import org.scalajs.dom.html
import org.querki.jquery._
import autowire._

import rx._

import querki.display.QuerkiUIUtils
import querki.display.input.InputGadget
import querki.globals._
import EditFunctions._
import querki.display.rx.RxInput

/**
 * The client side of the _checkList function.
 * 
 * Note that this replaces the old PickListGadget. We've deconstructed that, so this is much simpler.
 * 
 * @author jducoeur
 */
class CheckList(implicit e:Ecology) extends InputGadget[html.UList](e) with QuerkiUIUtils {

  lazy val Editing = interface[querki.editing.Editing]
  lazy val Pages = interface[querki.pages.Pages]

  val std = DataAccess.std

  def values = ???
  def doRender() = ???

  lazy val propId = $(elem).tidString("prop")

  // _checkList() may optionally specify a filter input and a new-item input, which may be the same thing.
  // Either way, it should be the id of an RxInput.
  lazy val filterId: Option[String] = $(elem).data("filterid").toOption.map(_.asInstanceOf[String])
  // _checkList() may specify a Model; if so, then typing a name and hitting Enter in the filter field will
  // produce a new Instance of that Model, with the Display Name set:
  lazy val modelIdOpt: Option[TID] = $(elem).data("modelid").toOption.map(s => TID(s.asInstanceOf[String]))

  lazy val allPicksByDisplay: Map[String, html.Element] = {
    // Index all of the list items by their display text, so that we can filter if desired:
    val pairs = $(elem).find("._pickName").mapElems { nameElement =>
      val text = nameElement.textContent
      // This .get is always a little suspicious, but it's hard to see how it could fail:
      val parent = $(nameElement).parent().get(0).get.asInstanceOf[html.Element]
      (text, parent)
    }
    pairs.toMap
  }

  var filterHook: Option[Obs] = None
  
  def saveCheckbox(checkbox:html.Element) = {
    val v = $(checkbox).valueString
    val checked = $(checkbox).prop("checked").asInstanceOf[Boolean]
    val path = Editing.propPath(propId, Some(thingId))
    val msg = 
      if (checked)
        AddToSet(path, v)
      else
        RemoveFromSet(path, v)
    saveChange(msg)
  }

  /**
   * Create a new element on the checklist. This is only present if the QL specifies addModel.
   */
  def quickCreate(modelId: TID)(name: String) = {
    val msg = ChangePropertyValue(Editing.propPath(std.basic.displayNameProp), List(name))

    for {
      thingInfo <- Client[EditFunctions].create(modelId, Seq(msg)).call()
      _ <- saveChange(AddToSet(path, thingInfo.oid.underlying))
    }
      StatusLine.showBriefly("Saved")
  }

  def hook() = {
    $(elem).find("._checkOption").change({ (evt:JQueryEventObject) =>
      val checkbox = evt.target.asInstanceOf[html.Element]
      saveCheckbox(checkbox)
    })

    // If we're filtering, connect to the filter field:
    filterId.map { id =>
      Pages.findPageFor(this).map { page =>
        page.gadgetListeners.listenFor(id) { filterGadget =>
          val rxInput = filterGadget.asInstanceOf[RxInput]
          implicit val ctx = page.ctx
          filterHook = Some {
            rxInput.textOpt.trigger {
              rxInput.textOpt.now match {
                case Some(filterText) => {
                  val txt = filterText.toLowerCase
                  allPicksByDisplay.foreach { case (text, e) =>
                    if (text.toLowerCase.contains(filterText))
                      $(e).show()
                    else
                      $(e).hide()
                  }
                }
                case None => {
                  allPicksByDisplay.values.foreach { e =>
                    $(e).show()
                  }
                }
              }
            }
          }

          // If the Model ID is specified, that means we want to be able to quick-create new entries on this list:
          modelIdOpt.map { modelId =>
            rxInput.onEnter(quickCreate(modelId))
          }
        }
      }
    }
  }  
}
