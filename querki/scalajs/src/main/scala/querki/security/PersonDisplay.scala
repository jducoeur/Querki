package querki.security

import org.scalajs.dom.html

import org.querki.jquery._
import scalatags.JsDom.all._
import rx._

import org.querki.gadgets._

import querki.api.StandardThings
import querki.globals._
import querki.util.ScalatagUtils

class PersonDisplay(
    showCls:String, 
    person:PersonInfo, 
    roleInfo:RoleInfo, 
    customInfo:RoleInfo,
    std: StandardThings)(implicit val ecology: Ecology, ctx:Ctx.Owner)
  extends Gadget[html.TableRow] with ScalatagUtils
{
  val customDisplay = GadgetRef.of[html.Div]

  var selected = false
  override def onCreate(e: html.TableRow) = {
    $(e).click { evt:JQueryEventObject =>
      if (selected) {
        $(e).removeClass("warning")
        $(e).addClass(showCls)
        selected = false
      } else {
        $(e).addClass("warning")
        $(e).removeClass(showCls)
        selected = true
      }
    }
  }
  
  def doRender() = {
    tr(cls:=showCls,
      td(
        MSeq(
          person.person.displayName,
          " -- ",
          new RolesDisplay(person.roles, person.person.oid, roleInfo, customInfo, customDisplay, std)
        ),
        customDisplay <= div(display := "None")
      )
    )
  }
}
  
