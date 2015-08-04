package querki.pages

import java.util.regex.Pattern

import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._

import org.querki.jquery._

import querki.display.rx._
import querki.imexport.ImportSpaceFunctions
import querki.globals._
// Needed in order to get evt.target.files:
import querki.photos.FileTarget._

/**
 * @author jducoeur
 */
class ImportSpacePage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  val fileInputElem = GadgetRef.of[dom.html.Input].
    whenRendered { inpGadget =>
      $(inpGadget.elem).change({ evt:JQueryEventObject =>
        val file = evt.target.files(0)
        if (Pattern.matches("text/xml", file.`type`)) {
          Client[ImportSpaceFunctions].importFromXML().call() foreach { path =>
            println(s"The upload actor can be found at $path")
          }
  //        val reader = new dom.FileReader()
  //        reader.onload = { uievt:dom.UIEvent => }
        } else {
          StatusLine.showBriefly(s"That is a ${file.`type`}. You can only upload images.")
        }
      })
    }
  
  def pageContent = {
    val guts =
      div(
        h1("Import a Space from a File"),
        p("To import an XML file that was exported from Querki, click here:"),
        // TODO: make this into a pretty custom button:
        fileInputElem <= input(/*cls:="_photoInputElem", */tpe:="file", accept:="text/xml")
      )
     
    Future.successful(PageContents("Import a Space", guts))
  }
}