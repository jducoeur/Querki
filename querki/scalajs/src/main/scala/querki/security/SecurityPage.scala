package querki.security

import org.scalajs.dom
import dom.html

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import autowire._
import rx._

import org.querki.gadgets._
import org.querki.jquery._

import models.Kind

import querki.data.ThingInfo
import querki.display.{ButtonGadget, RawDiv}
import querki.display.input.InputGadget
import querki.display.rx._
import querki.ecology._
import querki.editing.EditFunctions
import querki.globals._
import querki.pages._

import SecurityFunctions._

/**
 * This is lifted out so that EditSpaceInfoPage can also use it.
 */
trait LevelMap { page:Page =>
  
  lazy val levelMap:Map[SecurityLevel, TID] = 
    Map(
      (SecurityPublic -> std.security.public.oid),
      (SecurityMembers -> std.security.members.oid),
      (SecurityOwner -> std.security.owner.oid),
      (SecurityCustom -> TID("custom")),
      (SecurityInherited -> TID("inherit"))
    )
    
  def currentPermLevel(permInfo:PermInfo, thingPerm:Option[ThingPerm], isSpace:Boolean):SecurityLevel = {
    thingPerm.map(_.currently)
    // At the Space level, we show the defaults; otherwise, inherit from the Space:
    .getOrElse(if (isSpace) permInfo.default else SecurityInherited)
  }
  def currentPermOID(permInfo:PermInfo, thingPerm:Option[ThingPerm], isSpace:Boolean):String = {
    levelMap(currentPermLevel(permInfo, thingPerm, isSpace)).underlying
  }
}

/**
 * @author jducoeur
 */
class SecurityPage(params:ParamMap)(implicit val ecology:Ecology) 
  extends Page("security") with LevelMap 
{ self =>
  lazy val thingId = TID(params("thingId"))
  
  lazy val Client = interface[querki.client.Client]
  lazy val Editing = interface[querki.editing.Editing]

  class ShowPerms(t:ThingInfo, kindPerms:Seq[PermInfo], thingPerms:Seq[ThingPerm], isSpace:Boolean)(implicit val ecology:Ecology) extends Gadget[html.Div] {
    def doRender() =
      div(
        for (perm <- kindPerms) 
          yield div(cls:="row _permrow",
            div(cls:="col-md-2 _permname", b(perm.name)),
            new OnePerm(t, perm, Var(thingPerms.find(_.permId == perm.id)), isSpace, self)
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
    
    pageTitle = msg("pageTitle", ("thingName" -> thing.displayName))
    
    appliesToSpace = std.security.appliesToSpace.oid
    appliesToModels = std.security.appliesToModels.oid
    appliesToInstances = std.security.appliesToInstances.oid

    guts =
      div(
        h2(s"Security ", new ButtonGadget(ButtonGadget.Normal, id:="_doneButton", "Done")({ () => Pages.thingPageFactory.showPage(thing) })),
        
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
            } else
              p("""These are the permissions for this specific Instance. If you want to change the permissions for *all* Instances
                  |of this Model, go to the Instances Tab of the Security page for the Model.""".stripMargin),
            
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
    yield PageContents(pageTitle, guts)
}
