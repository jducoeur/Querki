package querki.editing

import scala.concurrent.Future

import org.scalajs.dom

import scalatags.JsDom.all._
import autowire._
import rx._

import querki.data.BasicThingInfo
import querki.display._
import querki.display.input._
import querki.display.rx._
import querki.editing.EditFunctions.PropertyChanged
import querki.globals._
import querki.pages._
import querki.security.SecurityFunctions
import SecurityFunctions._

class EditSpaceInfoPage(params:ParamMap)(implicit val ecology:Ecology) 
  extends Page("editSpaceInfo") with querki.security.LevelMap 
{  
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]
  
  lazy val spaceInfo = DataAccess.space.get
  
  /**
   * Specialized little gadget that exists solely to save the current read setting on a Thing.
   */
  class ReadSaver(info:BasicThingInfo) extends InputGadget[dom.html.Span](ecology) {
    override lazy val thingId = info.oid
    override val path = Editing.propPath(std.security.canReadPerm, Some(info))
    
    def values = securityRadio.flatMap { radio =>
      radio.selectedValOpt().map(Seq(_))
    }.getOrElse(Seq.empty)
    
    override def save() = {
      if (values.isEmpty)
        // Don't do anything unless something is actually selected:
        Future.successful(PropertyChanged)
      else
        super.save()
    }
    
    def doRender = span(display := "none")
    def hook() = {}
  }
  
  val spaceSaver = GadgetRef[ReadSaver]
  val spacePermsSaver = GadgetRef[ReadSaver]

  val securityRadio = GadgetRef[RxRadio]
    .whenRendered { g =>
      Obs(g.selectedValOpt, skipInitial=true) {
        // Whenever the selection changes, save both objects:
        spaceSaver.foreach(_.save())
        spacePermsSaver.foreach(_.save())
      }
    }
  
  implicit class RichThingPerms(perms:ThingPermissions) {
    // true iff *both* the Space itself and the Instances are set to this level:
    def readIs(level:SecurityLevel, readPerm:PermInfo):Boolean = {
      def find(ts:Seq[ThingPerm]) = ts.find(_.permId == std.security.canReadPerm.oid)
      
      (currentPermLevel(readPerm, find(perms.perms), true) == level) &&
      (currentPermLevel(readPerm, find(perms.instancePerms), true) == level)
    }
  }
  
  def oneRadioButton(level:SecurityLevel, lbl:String, chk:Boolean) = {
    RadioButton(levelMap(level).underlying, lbl, chk)
  }
  
  def pageContent = {
    for {
      // Use zip() to fire the Futures in parallel. Still kind of ugly, I'm afraid:
      (propEditors, (allPerms, spacePerms)) <-
        Editing.getSomePropertyEditors(spaceInfo.oid, 
          std.conventions.summaryProp,
          std.conventions.detailsProp,
          std.basic.displayNameProp).zip(
        Client[SecurityFunctions].getAllPerms().call().zip(
        Client[SecurityFunctions].permsFor(spaceInfo).call()))
      readPerm = allPerms.find { perm => perm.id.underlying == std.security.canReadPerm.underlying }.get
      isPublic = spacePerms.readIs(SecurityPublic, readPerm)
      isMembers = spacePerms.readIs(SecurityMembers, readPerm)
      guts =
        div(
          h1(cls:="_defaultTitle", "Edit Space Info"),
          
          p(cls:="_smallSubtitle", a(href:=Editing.advancedEditorFactory.pageUrl(spaceInfo), "Advanced Editor")),
          
          p("These are the most common properties to edit for your Space. When you are finished, press the Done button."),
          
          p(b("Space Name:")),
          propEditors(std.basic.displayNameProp),
          
          br(), p(b("Summary (a one-line description of this Space):")),
          propEditors(std.conventions.summaryProp),
          
          br(), p(b("Details (more information about this Space):")),
          propEditors(std.conventions.detailsProp),
          
          br(), p(b("Who can read this Space, broadly speaking?")),
          p(cls:="_smallSubtitle", "(You can make specific Things public or private using the Security page)"),
          spaceSaver <= new ReadSaver(spaceInfo),
          spacePermsSaver <= new ReadSaver(spacePerms.instancePermThing.get),
          securityRadio <= 
            new RxRadio("_securityRadio",
              oneRadioButton(SecurityPublic, "Everybody -- this Space is public", isPublic),
              oneRadioButton(SecurityMembers, "Only Members of the Space", isMembers),
              oneRadioButton(SecurityCustom, "Custom", (!isPublic && !isMembers))),
          
          br(), p(SpaceLinkButton(ButtonGadget.Primary, "Done"))
        )
    }
     yield PageContents(pageTitle, guts)
  }
}
