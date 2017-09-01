package querki.console

import org.scalajs.dom.html

import scalatags.JsDom.all._

import org.querki.gadgets._
import org.querki.jquery._

import querki.display.rx.{GadgetRef, RxTextAreaFixedSize}
import querki.globals._
import querki.pages._

import ConsoleFunctions._

class ConsolePage(params:ParamMap)(implicit val ecology:Ecology) extends Page() {
  
  val inputArea = GadgetRef[RxTextAreaFixedSize]
  val outputArea = GadgetRef.of[html.Div]
  
  override def afterRendered() = reset()
  
  def reset():Unit = {
    inputArea.mapElem { e =>
      // Ick: a fine illustration of where we need to improve GadgetRef. mapElem()
      // probably ought to be in a typeclass instead:
      val textArea = e.asInstanceOf[html.TextArea]
      $(textArea).value("> ")
      textArea.setSelectionRange(2, 2)
      $(textArea).focus()
    }
  }
  
  def pageContent =
    for {
      // For now, this page just starts -- it doesn't need any roundtrip:
      dummy <- Future.successful(())
      guts = 
        div(cls:="_console",
          h2("Querki Console"),
          outputArea <= div(cls:="_consoleOutput col-md-8 col-md-offset-2"),
          inputArea <= new RxTextAreaFixedSize(cls:="_consoleInput col-md-8 col-md-offset-2")
        )
    }
      yield PageContents("Querki Console", guts)
}
