package querki.pages

import java.util.regex.Pattern

import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._
import rx._

import org.querki.facades.fileupload._
import org.querki.jquery._

import querki.comm._
import querki.display.rx._
import querki.imexport.ImportSpaceFunctions
import querki.globals._
// Needed in order to get evt.target.files:
import querki.photos.FileTarget._
import querki.util.InputUtils

/**
 * @author jducoeur
 */
class ImportSpacePage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  val spaceName = GadgetRef[RxInput]
  
  val fileInputElem = GadgetRef.of[dom.html.Input].
    whenRendered { inpGadget =>
      $(inpGadget.elem).fileupload(FileUploadOptions
        .add({ (e:JQueryEventObject, data:FileUploadData) =>
          val file = data.files(0)
          if (Pattern.matches("text/xml", file.`type`)) {
            Client[ImportSpaceFunctions].importFromXML(spaceName.get.text()).call() foreach { path =>
              data.url = controllers.ClientController.upload(path).url
              val jqXHR = data.submit()
            }
          } else {
            StatusLine.showBriefly(s"That is a ${file.`type`}. You can only upload images.")
          }          
        })
        .multipart(false)
        .maxFileSize(5000000) // 5 MB
      )
    }
  
  def pageContent = {
    val guts =
      div(
        h1("Import a Space from a File"),
        p(spaceName <= new RxInput(Some(InputUtils.spaceNameFilter _), "text", cls:="form-control", placeholder:="Name of the new Space")),
        p("To import an XML file that was exported from Querki, click here:"),
        // TODO: make this into a pretty custom button:
        fileInputElem <= input(/*cls:="_photoInputElem", */tpe:="file", accept:="text/xml",
          disabled := Rx { spaceName.isEmpty || spaceName.get.text().length() == 0 })
      )
     
    Future.successful(PageContents("Import a Space", guts))
  }
}