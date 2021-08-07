package querki.console

import org.scalajs.dom.html

import scalatags.JsDom.all._
import autowire._

import org.querki.gadgets._
import org.querki.jquery._

import querki.display.ButtonGadget
import querki.display.rx.{RunButton, RxTextAreaFixedSize}
import querki.globals._
import querki.pages._

import ConsoleFunctions._

class ConsolePage(params: ParamMap)(implicit val ecology: Ecology) extends Page() {

  lazy val Client = interface[querki.client.Client]

  val inputArea = GadgetRef[RxTextAreaFixedSize]
  val outputArea = GadgetRef.of[html.Div]
  val submitButton = GadgetRef[RunButton]

  override def afterRendered() = reset()

  def reset(): Unit = {
    inputArea.mapElemNow { e =>
      // Ick: a fine illustration of where we need to improve GadgetRef. mapElem()
      // probably ought to be in a typeclass instead:
      val textArea = e.asInstanceOf[html.TextArea]
      $(textArea).value("> ")
      textArea.setSelectionRange(2, 2)
      $(textArea).focus()
    }
  }

  def leadSpaces(line: String): Int = {
    val i = line.indexWhere(_ != ' ')
    if (i == -1)
      0
    else
      i
  }

  def fixup(str: String) = {
    val lines = str.split("\n").toSeq
    val withBreaks = (Vector.empty[Modifier] /: lines) { (v, line) =>
      val leading = leadSpaces(line)
      val break =
        if (v.isEmpty)
          Vector.empty[Modifier]
        else
          Vector(br())
      v ++ break :+ raw("&nbsp;" * leading) :+ (line: Modifier)
    }
    MSeq(withBreaks)
  }

  def pageContent =
    for {
      // For now, this page just starts -- it doesn't need any roundtrip:
      dummy <- Future.successful(())
      guts =
        div(
          cls := "_console",
          h2("Querki Console"),
          outputArea <= div(cls := "_consoleOutput col-md-8 col-md-offset-2"),
          inputArea <= new RxTextAreaFixedSize(cls := "_consoleInput col-md-8 col-md-offset-2"),
          div(
            cls := "col-md-8 col-md-offset-2",
            submitButton <= RunButton(ButtonGadget.Primary, "Send", "Running...") { btn =>
              inputArea.mapElemNow { i =>
                val rawCmd = $(i).valueString
                val cmd =
                  if (rawCmd.startsWith("> "))
                    rawCmd.drop(2)
                  else
                    rawCmd
                Client[ConsoleFunctions].consoleCommand(cmd).call().map { result =>
                  val rendered = result match {
                    case DisplayTextResult(res) => p(fixup(res), cls := "_consoleOutMsg").render
                    case ErrorResult(msg)       => p("Error: ", fixup(msg), cls := "_consoleOutErr").render
                  }
                  outputArea.mapElemNow { o =>
                    $(o).append(p(rawCmd, cls := "_consoleOutCmd").render)
                    $(o).append(rendered)
                    $(o).scrollTop(o.scrollHeight)
                  }
                  btn.done()
                  reset()
                }
              }
            }
          )
        )
    } yield PageContents("Querki Console", guts)
}
