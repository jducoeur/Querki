package querki.security

import org.scalajs.dom
import dom.html

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import autowire._

import models.Kind

import querki.data.ThingInfo
import querki.display.Gadget
import querki.ecology._
import querki.globals._
import querki.pages._

import SecurityFunctions._

/**
 * @author jducoeur
 */
class SecurityPage(params:ParamMap)(implicit e:Ecology) extends Page(e, "security") with EcologyMember {
  
  lazy val thingId = TID(params("thingId"))
  
  lazy val Client = interface[querki.client.Client]
  
  class OnePerm(t:ThingInfo, permInfo:PermInfo, thingPerms:Seq[ThingPerm])(implicit val ecology:Ecology) extends Gadget[html.Div] {
    def doRender() =
      div(cls:="form-inline",
        if (permInfo.publicAllowed)
          div(cls:="_permcheckbox checkbox", label(input(tpe:="checkbox"), " Public")), " ",
        div(cls:="_permcheckbox checkbox", label(input(tpe:="checkbox"), " Members")), " ",
        div(cls:="_permcheckbox checkbox", label(input(tpe:="checkbox"), " Owner")), " ",
        div(cls:="_permcheckbox checkbox", label(input(tpe:="checkbox"), " Custom"))
      )
  }
  
  class ShowPerms(t:ThingInfo, kindPerms:Seq[PermInfo], thingPerms:Seq[ThingPerm])(implicit val ecology:Ecology) extends Gadget[html.Div] {
    def doRender() =
      div(
        for (perm <- kindPerms) 
          yield div(cls:="row _permrow",
            div(cls:="col-md-3 _permname", b(perm.name)),
            div(cls:="col-md-9", new OnePerm(t, perm, thingPerms)(ecology))
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
                p("""These are the permissions for the Space's main page only; use the Instances tab to manage
                    |the default permissions for the rest of the Space.""".stripMargin)
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
