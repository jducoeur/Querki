package querki.security

import org.scalajs.dom.html
import org.querki.jquery._
import scalatags.JsDom.all._
import rx._

import org.querki.gadgets._
import querki.api.StandardThings
import querki.display.QuerkiUIUtils
import querki.globals._
import querki.util.ScalatagUtils

class PersonDisplay(
    showCls:String,
    val person:PersonInfo,
    roleInfo:RoleInfo, 
    customInfo:RoleInfo,
    std: StandardThings)(implicit val ecology: Ecology, ctx:Ctx.Owner)
  extends Gadget[html.TableRow] with QuerkiUIUtils
{
  val customDisplay = GadgetRef.of[html.Div]

  val selected = Var(false)
  val cell = GadgetRef.of[html.TableCell]
    .whenRendered { g =>
      g.elemOpt.map { e =>
        $(e).click { evt:JQueryEventObject =>
          if (selected.now) {
            $(e).removeClass("warning")
            $(e).addClass(showCls)
            check.mapElemNow { checkElem =>
              $(checkElem).css("visibility", "hidden")
            }
            selected.update(false)
          } else {
            $(e).addClass("warning")
            $(e).removeClass(showCls)
            check.mapElemNow { checkElem =>
              $(checkElem).css("visibility", "visible")
            }
            selected.update(true)
          }
        }.children().click { evt:JQueryEventObject =>
          // Don't let child clicks propagate to the row itself:
          false
        }
      }
    }

  val check = GadgetRef.of[html.Span]

  def doRender() = {
    tr(cls:=showCls,
      cell <= td(
        check <= span(visibility := "hidden", icon("ok")),
        " ",
        MSeq(
          person.person.displayName,
          s" (${person.person.oid2.underlying}) -- ",
          new RolesDisplay(person.roles, person.person.oid, roleInfo, customInfo, customDisplay, std)
        ),
        customDisplay <= div(display := "None")
      )
    )
  }
}
  
