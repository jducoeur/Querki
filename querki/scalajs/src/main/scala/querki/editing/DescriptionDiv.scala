package querki.editing

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.data.BasicThingInfo
import querki.display.{Gadget, QText}
import querki.display.rx.{RxDiv, RxThingSelector}
import querki.util.ScalatagUtils

/**
 * This watches an RxThingSelector full of Things, and produces the div describing the currently-selected Thing.
 * 
 * @param selector Typically the selected() reactive of an RxSelect. We pass in this instead of the RxSelect itself
 *   so that you can orElse multiple RxSelects and feed the union into here.
 * @param immediate Set this to true iff the selector is already set to a valid value when this is created.
 */
class DescriptionDiv(page:ModelDesignerPage, selector:Rx[Option[(RxThingSelector, TID)]], immediate:Boolean = false)(implicit val ecology:Ecology) 
  extends EcologyMember with ScalatagUtils 
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  def thingLink = page.thingLink _
  
  val stdThingFut = DataAccess.standardThings
  val emptyDescription = span(raw("&nbsp;"))
  val selectedDescriptionObs = Obs(selector, skipInitial=(!immediate)) {
    selector() match {
      case Some((sel, oid)) => {
        val name = sel.selectedText()
        val fut = for {
          stdThings <- stdThingFut
          propMap <- page.propMapFut
          typeMap <- page.typeMapFut
          collMap <- page.collMapFut
          modelMap <- page.modelMapFut
          summaryOpt <- Client[ThingFunctions].getPropertyDisplay(oid, stdThings.conventions.summaryProp).call()
          detailsOpt <- Client[ThingFunctions].getPropertyDisplay(oid, stdThings.conventions.detailsProp).call()
        }
          yield
            // ... build the display of the Property info...
            div(
              b(name),
              propMap.get(oid).map { propInfo =>
                val typeInfoOpt:Option[BasicThingInfo] = typeMap.get(propInfo.typeId) orElse modelMap.get(propInfo.typeId)
                i(" (", thingLink(collMap(propInfo.collId)),
                  typeInfoOpt.map { typeInfo => MSeq(" of ", thingLink(typeInfo)) },
                  ")")
              },
              summaryOpt.map(summary => i(new QText(summary))),
              detailsOpt.map(details => new QText(details))
            )
            
        fut.foreach { desc => selectionDescription() = desc }
      }
        
      case None => selectionDescription() = emptyDescription
    }
  }
  
  val selectionDescription = Var[Gadget[dom.Element]](emptyDescription)
    
  val descriptionDiv = RxDiv(Rx {Seq(selectionDescription())})
}

object DescriptionDiv {
  implicit def getRxDiv(desc:DescriptionDiv):RxDiv = desc.descriptionDiv
}