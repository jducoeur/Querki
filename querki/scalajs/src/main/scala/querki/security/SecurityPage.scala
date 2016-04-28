package querki.security

import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import autowire._

import models.Kind

import querki.ecology._
import querki.globals._
import querki.pages._

/**
 * @author jducoeur
 */
class SecurityPage(params:ParamMap)(implicit e:Ecology) extends Page(e, "security") with EcologyMember {
  
  lazy val thingId = TID(params("thingId"))
  
  lazy val Client = interface[querki.client.Client]

  def pageContent = for {
    thing <- DataAccess.getThing(thingId)
    
    isSpace = (thing.kind == Kind.Space)
    isModel = thing.isModel
    hasInstancePerms = (isSpace || isModel)
    instancePermsOpt <-
      if (hasInstancePerms)
        Client[SecurityFunctions].instancePermsFor(thingId).call().map(Some(_))
      else
        Future.successful(None)
        
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
            }
          ),
          
          if (hasInstancePerms)
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
              }
            )
            
        )
        
      )
  }
    yield PageContents(s"Security for ${thing.displayName}", guts)
}
