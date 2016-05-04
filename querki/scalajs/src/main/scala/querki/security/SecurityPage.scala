package querki.security

import org.scalajs.dom
import dom.html

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import autowire._
import rx._

import org.querki.jquery._

import models.Kind

import querki.data.ThingInfo
import querki.display.{Gadget, RawDiv}
import querki.display.input.InputGadget
import querki.display.rx._
import querki.ecology._
import querki.editing.EditFunctions
import querki.globals._
import querki.pages._

import SecurityFunctions._

/**
 * @author jducoeur
 */
class SecurityPage(params:ParamMap)(implicit e:Ecology) extends Page(e, "security") with EcologyMember {
  
  lazy val thingId = TID(params("thingId"))
  
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]
  
  lazy val levelMap:Map[SecurityLevel, TID] = 
    Map(
      (SecurityPublic -> std.security.public.oid),
      (SecurityMembers -> std.security.members.oid),
      (SecurityOwner -> std.security.owner.oid),
      (SecurityCustom -> TID(""))
    )
  
  class OnePerm(t:ThingInfo, permInfo:PermInfo, thingPerm:Option[ThingPerm]) extends InputGadget[html.Div](ecology) {
    
    lazy val customDisplay = $(elem).find("._permCustom")
    
    def showCustom() = {
      Client[EditFunctions].getOnePropertyEditor(t.oid, permInfo.id).call().foreach { propEditInfo =>
        customDisplay.empty()
        customDisplay.append(new RawDiv(propEditInfo.editor)(ecology).render)
        Gadgets.hookPendingGadgets()
        updatePage()
      }
      customDisplay.show()
    }
    
    def hook() = {
      $(elem).find("._permRadio").click { radio:dom.Element =>
        val oid = $(radio).valueString
        if (oid.length == 0) {
          // Custom button, so open the Custom pane. Note that this has its own inherent Save built in:
          showCustom()
        } else {
          $(elem).find("._permCustom").hide()
          // Does this work? Is Rx sufficiently synchronous to call save() right after it?
          currently() = oid
          save()
        }
      }
      
      if (isCustom())
        showCustom
    }
    
    override lazy val thingId = t.oid
    override val path = Editing.propPath(permInfo.id, Some(t))
    
    val currently =
      Var(levelMap(thingPerm.map(_.currently).getOrElse(permInfo.default)).underlying)
    val isCustom = Rx { currently().length() == 0 }
      
    def values = List(currently())
    
    def makeBox(lbl:String, level:SecurityLevel) = {
      div(
        cls:="_permcheckbox col-md-2", 
        label(cls:="radio-inline", 
          input(cls:="_permRadio", tpe:="radio", name:=path, 
            if (currently() == levelMap(level).underlying)
              checked:="checked", 
            value:=levelMap(level).underlying),
          s" $lbl"))
    }
    
    implicit def rxAttr = new RxAttr[String]
    
    def doRender() =
      div(cls:="form-inline",
        if (permInfo.publicAllowed)
          makeBox("Public", SecurityPublic)
        else
          div(cls:="_permcheckbox col-md-2", label(" ")),
        makeBox("Members", SecurityMembers),
        makeBox("Owner", SecurityOwner),
        makeBox("Custom", SecurityCustom),
        
        div(cls:="_permCustom col-md-offset-3 col-md-8", display:="none", "Loading...")
      )
  }
  
  class ShowPerms(t:ThingInfo, kindPerms:Seq[PermInfo], thingPerms:Seq[ThingPerm])(implicit val ecology:Ecology) extends Gadget[html.Div] {
    def doRender() =
      div(
        for (perm <- kindPerms) 
          yield div(cls:="row _permrow",
            div(cls:="col-md-3 _permname", b(perm.name)),
            new OnePerm(t, perm, thingPerms.find(_.permId == perm.id))
          )
      )
  }
  
  def filterPermsFor(allPerms:Seq[PermInfo], target:TID):Seq[PermInfo] = {
    allPerms.filter(_.appliesTo.contains(target))
  }

  def pageContent = for {
    thing <- DataAccess.getThing(thingId)
    allPerms <- Client[SecurityFunctions].getAllPerms().call()
    
    isSpace = (thing.kind == Kind.Space)
    isModel = thing.isModel
    hasInstancePerms = (isSpace || isModel)
    perms <- Client[SecurityFunctions].permsFor(thingId).call()
    
    appliesToSpace = std.security.appliesToSpace.oid
    appliesToModels = std.security.appliesToModels.oid
    appliesToInstances = std.security.appliesToInstances.oid

    guts =
      div(
        h2(s"Security"),
        
        ul(cls:="nav nav-tabs", role:="tablist",
          li(role:="presentation", cls:="active", a(href:="#secThis", role:="tab", "data-toggle".attr:="tab", thing.displayName)),
          if (hasInstancePerms)
            li(role:="presentation", a(href:="#secInst", role:="tab", "data-toggle".attr:="tab", "Instances"))
        ),
          
        div(cls:="tab-content",
          section(role:="tabpanel", cls:="tab-pane active", id:="secThis",
            h3(s"Permissions for ${thing.displayName}"),
            
            if (hasInstancePerms) {
              if (isSpace)
                p("""These are the permissions for the Space's main page, and a few Space-wide permissions; use the Instances tab to manage
                    |the default permissions for Instances in this Space.""".stripMargin)
              else
                p("""These are the permissions for this Model; use the Instances tab to manage the permissions
                    |for its Instances.""".stripMargin)
            },
            
            if (isSpace)
              new ShowPerms(thing, filterPermsFor(allPerms, appliesToSpace), perms.perms)
            else if (isModel)
              new ShowPerms(thing, filterPermsFor(allPerms, appliesToModels), perms.perms)
            else
              new ShowPerms(thing, filterPermsFor(allPerms, appliesToInstances), perms.perms)
          ),
          
          if (hasInstancePerms && perms.instancePermThing.isDefined)
            section(role:="tabpanel", cls:="tab-pane", id:="secInst",
              if (isSpace) {
                MSeq(
                  h3(s"Permissions for Instances in Space ${thing.displayName}"),
                  
                  p("""These are the permissions to use for *all* Instances in this Space, unless the
                      |Model or Instance says otherwise.""".stripMargin)
                )
              } else {
                MSeq(
                  h3(s"Permissions for Instances of ${thing.displayName}"),
                  
                  p("""These are the permissions to use for all of this Model's Instances, unless the
                      |Instance itself says otherwise.""".stripMargin)
                )
              },
              
              new ShowPerms(perms.instancePermThing.get, filterPermsFor(allPerms, appliesToInstances), perms.instancePerms)
            )
            
        )
        
      )
  }
    yield PageContents(s"Security for ${thing.displayName}", guts)
}
