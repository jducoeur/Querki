package querki.security

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._

import org.querki.gadgets._

import querki.data.ThingInfo
import querki.globals._
import querki.display.input._

class EditRolePanel(
    role: ThingInfo
  )(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends Gadget[html.Div] with EcologyMember
{
  def doRender() = 
    div(
      // TODO: text input for the name of the role; use TextInputGadget with NoAutoSave
      // TODO: list of checkboxes for the permissions of the role
      // TODO: list of the Shared Invites using this Role
      // TODO: modifiable list of the Members with this role
      // TODO: Save button, which takes all of the inputs, accumulates their values, and saves
      //   them at one shot
    )
}
