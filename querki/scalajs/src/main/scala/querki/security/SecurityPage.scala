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
import querki.display.{ButtonGadget, Gadget, RawDiv}
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
      (SecurityCustom -> TID("custom")),
      (SecurityInherited -> TID("inherit"))
    )
  
  class OnePerm(t:ThingInfo, permInfo:PermInfo, thingPerm:Option[ThingPerm], isSpace:Boolean) extends InputGadget[html.Div](ecology) {
    
    lazy val customDisplay = $(elem).find("._permCustom")
    
    def updateCustom() = {
      if (isCustom()) {
        Client[EditFunctions].getOnePropertyEditor(t.oid, permInfo.id).call().foreach { propEditInfo =>
          customDisplay.empty()
          customDisplay.append(new RawDiv(propEditInfo.editor)(ecology).render)
          Gadgets.hookPendingGadgets()
          updatePage()
        }
        customDisplay.show()
      } else {
        customDisplay.hide()
      }
    }
    
    def hook() = {
      $(elem).find("._permRadio").click { radio:dom.Element =>
        currently() = $(radio).valueString
        if (!isCustom()) {
          if (isInherit()) {
            // The meaning of "inherited" is that we don't have the Property at all
            // TODO: in principle, this belongs in InputGadget. But that implies that we need
            // to create a new value of PropertyChange for removing a Property, with back-end
            // support for that.
            StatusLine.showUntilChange("Saving...")
            Client[EditFunctions].removeProperty(t.oid, permInfo.id).call().foreach { response =>
              StatusLine.showBriefly("Saved")
            }
          } else {
            // It's a standard value -- the OID of Public, Members or Owner -- so save that:
            save()
          }
        }
        // Show or hide the customDisplay as appropriate:
        updateCustom()
      }

      updateCustom()
    }
    
    override lazy val thingId = t.oid
    override val path = Editing.propPath(permInfo.id, Some(t))
    
    val currently =
      Var(
        levelMap(
          thingPerm
          .map(_.currently)
          // At the Space level, we show the defaults; otherwise, inherit from the Space:
          .getOrElse(if (isSpace) permInfo.default else SecurityInherited)
        ).underlying)
    val isCustom = Rx { currently() == "custom" }
    val isInherit = Rx { currently() == "inherit" }
      
    // Note that we don't save(), and thus don't use this, if it is custom or inherit:
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
          div(cls:="col-md-2", label(" ")),
        makeBox("Members", SecurityMembers),
        makeBox("Owner", SecurityOwner),
        makeBox("Custom", SecurityCustom),
        if (!isSpace)
          makeBox("Inherit", SecurityInherited),
        
        div(cls:="_permCustom col-md-offset-2 col-md-8", display:="none", "Loading...")
      )
  }
  
  class ShowPerms(t:ThingInfo, kindPerms:Seq[PermInfo], thingPerms:Seq[ThingPerm], isSpace:Boolean)(implicit val ecology:Ecology) extends Gadget[html.Div] {
    def doRender() =
      div(
        for (perm <- kindPerms) 
          yield div(cls:="row _permrow",
            div(cls:="col-md-2 _permname", b(perm.name)),
            new OnePerm(t, perm, thingPerms.find(_.permId == perm.id), isSpace)
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
        h2(s"Security ", new ButtonGadget(ButtonGadget.Normal, "Done")({ () => Pages.thingPageFactory.showPage(thing) })),
        
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
              new ShowPerms(thing, filterPermsFor(allPerms, appliesToSpace), perms.perms, isSpace)
            else if (isModel)
              new ShowPerms(thing, filterPermsFor(allPerms, appliesToModels), perms.perms, isSpace)
            else
              new ShowPerms(thing, filterPermsFor(allPerms, appliesToInstances), perms.perms, isSpace)
          ),
          
          if (hasInstancePerms && perms.instancePermThing.isDefined)
            section(role:="tabpanel", cls:="tab-pane", id:="secInst",
              if (isSpace) {
                MSeq(
                  h3(s"Permissions for Instances in Space ${thing.displayName}"),
                  
                  p("""These are the permissions to use for *all* Things in this Space, unless the
                      |Model or Instance says otherwise. If a Model has a permission marked
                      |"Inherit", it will use this value. If an Instance's permission says "Inherit", it
                      |will use this value unless the Model's Instances page says otherwise.""".stripMargin)
                )
              } else {
                MSeq(
                  h3(s"Permissions for Instances of ${thing.displayName}"),
                  
                  p("""These are the permissions to use for all of this Model's Instances, unless the
                      |Instance itself says otherwise. If an Instance's permissions are marked "Inherit"
                      |(which they usually are), it will use these values.""".stripMargin)
                )
              },
              
              new ShowPerms(perms.instancePermThing.get, filterPermsFor(allPerms, appliesToInstances), perms.instancePerms, isSpace)
            )
            
        )
        
      )
  }
    yield PageContents(s"Security for ${thing.displayName}", guts)
}
