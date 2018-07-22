package querki.security

import scala.concurrent.Future

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._
import autowire._

import org.querki.facades.bootstrap._
import org.querki.gadgets._
import org.querki.jquery._

import querki.api.{StandardThings, ThingFunctions}
import querki.data.{PropValInfo, ThingInfo}
import querki.display.ButtonGadget
import querki.display.input._
import querki.editing.EditFunctions
import querki.editing.EditFunctions._
import querki.globals._
import querki.security.SecurityFunctions._

private[security] class PermCheckboxes(allPerms: Seq[PermInfo], thingPropsOpt: Option[Seq[PropValInfo]], std: StandardThings) 
  (implicit e: Ecology, ctx: Ctx.Owner)
  extends InputGadget[html.Div](e) with NoAutoSave with ForProp 
{
  val prop = std.security.rolePermissionsProp
  
  def hook() = {}
  
  def values: Seq[String] = 
    checkboxes
      .filter(oneChk => $(oneChk.chk.elem).prop("checked").asInstanceOf[Boolean])
      .map(_.perm.id.underlying)
  
  // TODO: I'd be much more comfortable with this if we had a good way to get OIDs instead of names here.
  // It works the way it does because that's what we get from ThingFunctions.getProperties(), but it
  // kind of sucks.
  val thingPermNames: List[String] = {
    (for {
      thingProps <- thingPropsOpt
      rolePermsProp <- thingProps.find(_.propInfo.oid == std.security.rolePermissionsProp.oid)
    }
      yield rolePermsProp.raw.lines.toList).getOrElse(List())
  }
  
  def hasPerm(perm: PermInfo) = thingPermNames.contains(perm.name)
  
  class OneCheckbox(val perm: PermInfo) extends Gadget[html.Div] {
    val name = GadgetRef.of[html.Anchor]
      .whenRendered { g =>
        $(g.elem).popover(PopoverOptions.trigger(Trigger.click))
      }
    val chk = GadgetRef.of[html.Input]
    
    def doRender() =
      div(cls := "row",
        span(cls := "col-md-1",
          chk <= input(
            tpe := "checkbox",
            if (hasPerm(perm))
              checked := "checked"
          )
        ),
        name <= a(cls := "", data("toggle") := "popover", data("content") := perm.summary, perm.name)
      )
  }
  
  val checkboxes = 
    for (perm <- allPerms)
      yield new OneCheckbox(perm)
  
  def doRender() =
    div(cls := "container",
      p(b("This Role gives its members these permissions throughout this Space")),
      checkboxes
    )
}

/**
 * The panel for editing or creating a Role. Don't create this directly; use the helper functions in the
 * companion object. (Needed because we may need to fetch info from the server to create this.)
 * 
 * This panel is a bit experimental, in that it uses an explicit Save button. I'm not entirely sure that
 * I like this, now that I've done it -- it's inconsistent with the rest of Querki.
 */
private[security] class EditRolePanel(
    allPerms: Seq[PermInfo],
    roleOpt: Option[ThingInfo],
    rolePropsOpt: Option[Seq[PropValInfo]],
    parent: RoleEditCompleter
  )(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends Gadget[html.Div] with EcologyMember
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  
  lazy val std = DataAccess.std
  
  type InputGadgetRef = GadgetRef[InputGadget[_]]
  
  val nameInput = GadgetRef[InputGadget[_]]
  val permCheckboxes = GadgetRef[InputGadget[_]]
  
  val fields: List[InputGadgetRef] = List(nameInput, permCheckboxes)
  
  val creating = roleOpt.isEmpty
  def initialName = roleOpt.map(_.displayName).getOrElse("")
  
  def changeMsgs(): List[PropertyChange] = {
    def oneSaveMsg(ref: InputGadgetRef): Option[PropertyChange] = ref.mapNow(_.propertyChangeMsg())
    fields.map(oneSaveMsg).flatten
  }
  def saveMsg(): PropertyChange = {
    MultiplePropertyChanges(changeMsgs())
  }
  
  def doRender() = 
    div(cls := "panel panel-default",
      div(cls := "panel-heading",
        div(
          if (creating)
            "Create new Role"
          else
            span(b(initialName))
        )
      ),
      div(cls := "panel-body",
        form(
          div(cls := "form-group",
            label("Role Name"),
            nameInput <= 
              new TextInputGadget(Seq("form-control", "col-md-3"), value := initialName) 
                with NoAutoSave
                with ForProp { val prop = std.basic.displayNameProp }
          ),
          
          permCheckboxes <= new PermCheckboxes(allPerms, rolePropsOpt, std),
          
          // TODO: list of the Shared Invites using this Role
          // TODO: modifiable list of the Members with this role
          
          div(
            new ButtonGadget(ButtonGadget.Primary, "Save")({() => 
              roleOpt match {
                case Some(role) => {
                  for {
                    result <- InputGadget.doSaveChange(role.oid, saveMsg())
                    newRole <- Client[ThingFunctions].getThingInfo(role.oid).call()
                  }
                    parent.roleComplete(Some(newRole))
                }
                case None => {
                  for {
                    // TODO: This really ought to take a single PropertyChange, now that we have
                    // MultiplePropertyChanges:
                    newRole <- Client[EditFunctions].create(std.security.customRoleModel, changeMsgs()).call()
                  }
                    parent.roleComplete(Some(newRole))
                }
              }
            }),
            
            " ",
            new ButtonGadget(ButtonGadget.Normal, "Cancel")({() =>
              parent.roleComplete(None)
            })
          )
        )
      )
    )
}

object EditRolePanel {
  def prepToEdit(role: ThingInfo, parent: RoleEditCompleter)(implicit ecology: Ecology, ctx: Ctx.Owner): Future[EditRolePanel] = {
    val Client = ecology.api[querki.client.Client]
    
    for {
      allPerms <- Client[SecurityFunctions].getAllPerms().call()
      thingProps <- Client[ThingFunctions].getProperties(role).call()
    }
      yield new EditRolePanel(allPerms, Some(role), Some(thingProps), parent)
  }
  
  def create(parent: RoleEditCompleter)(implicit ecology: Ecology, ctx: Ctx.Owner): Future[EditRolePanel] = {
    val Client = ecology.api[querki.client.Client]
   
    for {
      allPerms <- Client[SecurityFunctions].getAllPerms().call()      
    }
      yield new EditRolePanel(allPerms, None, None, parent)
  }
}
