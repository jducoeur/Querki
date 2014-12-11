package querki.editing

import org.scalajs.dom
import scalatags.JsDom.all._
import rx._
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.display.{Gadget, QText}
import querki.display.rx.{RxDiv, RxThingSelector}

/**
 * This watches an RxThingSelector full of Things, and produces the div describing the currently-selected Thing.
 * 
 * @param selector Typically the selected() reactive of an RxSelect. We pass in this instead of the RxSelect itself
 *   so that you can orElse multiple RxSelects and feed the union into here.
 */
class DescriptionDiv(selector:Rx[Option[(RxThingSelector, String)]])(implicit val ecology:Ecology) extends EcologyMember {
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  val stdInfoFut = DataAccess.standardInfo
  val emptyDescription = span(raw("&nbsp;"))
  val selectedDescriptionObs = Obs(selector, skipInitial=true) {
    selector() match {
      case Some((sel, oid)) => {
        val name = sel.selectedText()
        val fut = for {
          stdInfo <- stdInfoFut
          summaryOpt <- Client[ThingFunctions].getPropertyDisplay(oid, stdInfo.summaryPropId).call()
          detailsOpt <- Client[ThingFunctions].getPropertyDisplay(oid, stdInfo.detailsPropId).call()
        }
          yield
            // ... build the display of the Property info...
            div(
              b(name),
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
