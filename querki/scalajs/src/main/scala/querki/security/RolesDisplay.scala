package querki.security

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._

import org.querki.gadgets._
import org.querki.gadgets.core.GadgetElementRef

import querki.api.StandardThings
import querki.data.TID
import querki.display.input.InputGadget
import querki.globals._
import querki.util.ScalatagUtils

class RolesDisplay(
  initialRoles:Seq[TID], 
  tid:TID, 
  roleInfo:RoleInfo, 
  customInfo:RoleInfo, 
  val selectorArea: GadgetElementRef[html.Div],
  std: StandardThings
  )(implicit e: Ecology, ctx:Ctx.Owner)
  extends InputGadget[html.Span](e) with ScalatagUtils
{
  lazy val Editing = interface[querki.editing.Editing]
  
  override lazy val thingId = tid
  override def path = Editing.propPath(std.security.personRolesProp.oid, Some(thingId))
  def values = roleDisplay.curValue.toList ++ customDisplayRef.opt.now.map(_.currentRoleIds.now.toList).getOrElse(List()) 
  
  val roleDisplay = new StandardRoleDisplay(this, initialRoles, tid, roleInfo)
  val customDisplayRef = GadgetRef[CustomRoleDisplay]
  
  def doRender() =
    span(
      roleDisplay,
      if (!customInfo.isEmpty) {
        MSeq(
          " and ",
          customDisplayRef <= new CustomRoleDisplay(this, initialRoles, tid, customInfo))
      }
    )
    
  def hook() = {}
}
