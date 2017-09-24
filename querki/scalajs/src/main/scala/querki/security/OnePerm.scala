package querki.security

import org.scalajs.dom
import dom.html

import scalatags.JsDom.all._
import rx._
import autowire._

import org.querki.jquery._

import org.querki.gadgets._

import querki.data.BasicThingInfo
import querki.display.{ButtonGadget, RawDiv}
import querki.display.input.InputGadget
import querki.display.rx._
import querki.editing.EditFunctions
import querki.globals._
import querki.pages.Page

import SecurityFunctions._

class OnePerm(t:BasicThingInfo, permInfo:PermInfo, thingPerm:Rx[Option[ThingPerm]], isSpace:Boolean, page:Page with LevelMap)
  (implicit e:Ecology, ctx:Ctx.Owner) extends InputGadget[html.Div](e)
{
  lazy val Editing = interface[querki.editing.Editing]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val Pages = interface[querki.pages.Pages]
  
  lazy val customDisplay = $(elem).find("._permCustom")
  
  def updateCustom() = {
    if (isCustom.now) {
      Client[EditFunctions].getOnePropertyEditor(t.oid, permInfo.id).call().foreach { propEditInfo =>
        customDisplay.empty()
        customDisplay.append(new RawDiv(propEditInfo.editor)(ecology).render)
        Gadgets.hookPendingGadgets()
        Pages.updatePage(this)
      }
      customDisplay.show()
    } else {
      customDisplay.hide()
    }
  }
  
  val saving = Var(false)
  
  def hook() = {
    $(elem).find("._permRadio").click { radio:dom.Element =>
      currently() = $(radio).valueString
      if (!isCustom.now) {
        // Note that, instead of using the usual "Saving/Saved" affordance, we actually disable/re-enable
        // the permissions while saving, via the reactive "saving" flag. This is specifically to have a
        // reliable way to *test* this, and to prevent weird races.
        saving() = true
        val changeFut = if (isInherit.now) {
          // The meaning of "inherited" is that we don't have the Property at all
          // TODO: in principle, this belongs in InputGadget. But that implies that we need
          // to create a new value of PropertyChange for removing a Property, with back-end
          // support for that.
          Client[EditFunctions].removeProperty(t.oid, permInfo.id).call()
        } else {
          // It's a standard value -- the OID of Public, Members or Owner -- so save that:
          save()
        }
        changeFut.foreach { response =>
          saving() = false
        }
      }
      // Show or hide the customDisplay as appropriate:
      updateCustom()
    }

    updateCustom()
  }
  
  override lazy val thingId = t.oid
  override val path = Editing.propPath(permInfo.id, Some(t))
  // HACK: this name is what the Functional Tests expect. When we get paths to be
  // more consistent, replace this with plain old path:
  val namePath = Editing.propPathOldStyleHack(permInfo.id, Some(t))
  
  // TODO: the relationship of thingPerm and currently is clumsy. We really should be pushing the
  // changes going through currently() back into thingPerm, and that should be feeding directly
  // back to the server.
  def calcCurrently() = page.currentPermOID(permInfo, thingPerm.now, isSpace)
  val currently = Var(calcCurrently())
  val isCustom = Rx { currently() == "custom" }
  val isInherit = Rx { currently() == "inherit" }
  val thingWatcher = thingPerm.triggerLater {
    currently() = page.currentPermOID(permInfo, thingPerm.now, isSpace)
  }
    
  // Note that we don't save(), and thus don't use this, if it is custom or inherit:
  def values = List(currently.now)
  
  class OneBoxGadget(lbl:String, level:SecurityLevel) extends Gadget[html.Div] {
    val box = GadgetRef.of[html.Input]
    
    val watcher = currently.trigger {
      box.mapElemNow { e =>
        $(e).prop("checked", (currently.now == page.levelMap(level).underlying))
      }
    }
    
    def doRender() = 
      div(
        cls:="_permcheckbox col-md-2", 
        label(cls:="radio-inline", 
          box <= input(cls:="_permRadio", tpe:="radio", name:=namePath, 
            if (currently.now == page.levelMap(level).underlying) checked:="checked",
            value:=page.levelMap(level).underlying,
            disabled := saving),
          s" $lbl"))
  }
  
  def doRender() =
    div(cls:="form-inline",
      if (permInfo.publicAllowed)
        new OneBoxGadget("Public", SecurityPublic)
      else
        div(cls:="col-md-2", label(" ")),
      new OneBoxGadget("Members", SecurityMembers),
      new OneBoxGadget("Owner", SecurityOwner),
      new OneBoxGadget("Custom", SecurityCustom),
      if (!isSpace)
        new OneBoxGadget("Inherit", SecurityInherited),
      
      div(cls:="_permCustom col-md-offset-2 col-md-8", display:="none", "Loading...")
      )
}
 
