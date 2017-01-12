package querki.editing

import scala.concurrent.Future

import scalatags.JsDom.all._

import querki.display._
import querki.display.input._
import querki.globals._
import querki.pages._

class EditSpaceInfoPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("editSpaceInfo") {
  
  lazy val Editing = interface[querki.editing.Editing]
  
  lazy val spaceInfo = DataAccess.space.get
  
  def pageContent = {
    for {
      propEditors <- 
        Editing.getSomePropertyEditors(spaceInfo.oid, 
          std.apps.summaryProp,
          std.apps.detailsProp)
      guts =
        div(
          h1("Edit Space Info"),
          
          p(cls:="_smallSubtitle", a(href:=Editing.advancedEditorFactory.pageUrl(spaceInfo), "Advanced Editor")),
          
          p("These are the most common properties to edit for your Space. When you are finished, press the Done button."),
          
          br(), p(b("Summary (a one-line description of this Space):")),
          propEditors(std.apps.summaryProp),
          
          br(), p(b("Details (more information about this Space):")),
          propEditors(std.apps.detailsProp),
          
          br(), p(SpaceLinkButton(ButtonGadget.Primary, "Done"))
        )
    }
     yield PageContents(pageTitle, guts)
  }
}
