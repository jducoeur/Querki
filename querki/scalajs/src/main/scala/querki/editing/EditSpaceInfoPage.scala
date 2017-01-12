package querki.editing

import scala.concurrent.Future

import scalatags.JsDom.all._
import autowire._

import querki.display._
import querki.display.input._
import querki.display.rx._
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
  
  val securityRadio = GadgetRef[RxRadio]
  
  implicit class RichThingPerms(perms:ThingPermissions) {
    // true iff *both* the Space itself and the Instances are set to this level:
    def readIs(level:SecurityLevel, readPerm:PermInfo):Boolean = {
      def find(ts:Seq[ThingPerm]) = ts.find(_.permId == std.security.canReadPerm)
      
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
      readPerm = allPerms.find{perm => println(s"Trying ${perm.id}"); perm.id.underlying == std.security.canReadPerm.underlying}.get
      isPublic = spacePerms.readIs(SecurityPublic, readPerm)
      isMembers = spacePerms.readIs(SecurityMembers, readPerm)
      _ = println(s"Public: $isPublic Members: $isMembers")
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
