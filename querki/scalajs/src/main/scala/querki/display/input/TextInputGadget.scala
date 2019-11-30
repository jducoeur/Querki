package querki.display.input

import org.querki.gadgets.core.GadgetRef

import scala.scalajs.js
import js._
import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import org.querki.jsext._
import scalatags.JsDom.all._
import querki.globals._

import scala.scalajs.js.annotation.{JSGlobal, JSImport}

class TextInputGadget(classes:Seq[String], mods:Modifier*)(implicit e:Ecology) 
  extends InputGadget[dom.HTMLInputElement](e) with querki.display.QuerkiUIUtils
{
  
  def values = List(elem.value)
  
  // TBD: do we need an unhook, to avoid leaks?
  def hook() = {
    $(elem).change({ e:dom.Element => save() })
    
    $(elem).keypress({ (evt:JQueryEventObject) => beginChanges() })
  }
  
  def doRender() =
    input(classes(classes :+ "_textEdit"), tpe:="text", mods)
    
}

@js.native
trait AutosizeFacade extends JQuery {
  def autosize():JQuery = js.native
}
object AutosizeFacade {
  implicit def jq2Autosize(jq:JQuery):AutosizeFacade = jq.asInstanceOf[AutosizeFacade]
}
import AutosizeFacade._

@js.native
trait JQueryEventEnhanced extends js.Object {
  // This should be a standard part of JQueryEventObject, IMO:
  def ctrlKey:UndefOr[Boolean] = js.native
}
object JQueryEventEnhanced {
  implicit def jqe2Enhanced(evt:JQueryEventObject):JQueryEventEnhanced = evt.asInstanceOf[JQueryEventEnhanced]
}
import JQueryEventEnhanced._

@js.native
trait CodeMirrorInstance extends js.Object {

}

@js.native
@JSImport("CodeMirror", JSImport.Default, globalFallback = "CodeMirror")
object CodeMirror extends js.Object {
  def fromTextArea(elem: dom.HTMLTextAreaElement, opts: CodeMirrorOptions): CodeMirrorInstance = js.native
}

@js.native
trait CodeMirrorOptions extends js.Object
object CodeMirrorOptions extends CodeMirrorOptionBuilder(noOpts)
class CodeMirrorOptionBuilder(val dict: OptMap) extends JSOptionBuilder[CodeMirrorOptions, CodeMirrorOptionBuilder](new CodeMirrorOptionBuilder(_)) {
  def addModeClass(v: Boolean) = jsOpt("addModeClass", v)
  def lineWrapping(v: Boolean) = jsOpt("lineWrapping", v)
  def mode(v: String) = jsOpt("mode", v)
  def scrollbarStyle(v: String) = jsOpt("scrollbarStyle", v)
}

class LargeTextInputGadget(mods:Modifier*)(implicit e:Ecology) extends InputGadget[dom.HTMLTextAreaElement](e) {
  def values: List[String] = ???

  println(s"====> In the new LargeTextInputGadget")

  def hook() = {
    println(s"====> In hook()")

    val opts =
      CodeMirrorOptions
        .mode("qltext-outer")
        .addModeClass(true)
        .lineWrapping(true)
    println(s"====> About to get the TextArea")
    CodeMirror.fromTextArea($(elem).get(0).get.asInstanceOf[dom.HTMLTextAreaElement], opts)
    println(s"====> It should now be CodeMirror")
  }

  override def setValue(v:String):Unit = {
    println(s"Setting value to $v")
  }

  val initialArea = GadgetRef.of[dom.HTMLTextAreaElement]

  def doRender() =
    textarea(cls:="_largeTextEdit", mods)

}

//class LargeTextInputGadget(mods:Modifier*)(implicit e:Ecology) extends InputGadget[dom.HTMLTextAreaElement](e) {
//
//  def values = List(elem.value)
//
//  override def setValue(v:String):Unit = {
//    $(elem).value(v).trigger("autosize.resize")
//    save()
//  }
//
//  // TBD: do we need an unhook, to avoid leaks?
//  def hook() = {
//    $(elem).addClass("col-md-10")
//
//    // Mark LargeTextInputs as autosized.
//    // We specifically need to *not* apply autosize to the template elements, or else it won't
//    // successfully apply to them when we actually instantiate them.
//    // Note that we define the :notUnder selector in PageManager:
//    $(elem).filter(":notUnder(.inputTemplate)").autosize()
//
//    $(elem).change({ e:dom.Element => save() })
//
//    // Intercept ctrl-s, and save the value of this text. This is a bit horrible, but
//    // necessary in order to work cross-browser.
//    // TBD: should we do this at the Page level, so that it does the right thing anywhere
//    // on the page? If nothing else, it would block the annoying popup.
//    $(elem).keydown({ (evt:JQueryEventObject) =>
//      val metaKey = evt.metaKey.asInstanceOf[Boolean]
//      if ((metaKey || (evt.ctrlKey.isDefined && evt.ctrlKey.get))
//          && (evt.which.toChar.toString.toLowerCase == "s"))
//      {
//        evt.preventDefault()
//        save()
//      }
//    })
//
//    $(elem).keypress({ (evt:JQueryEventObject) => beginChanges() })
//  }
//
//  def doRender() =
//    textarea(cls:="_largeTextEdit", mods)
//
//}
