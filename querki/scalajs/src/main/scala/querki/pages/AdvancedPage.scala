package querki.pages

import org.scalajs.dom

import scalatags.JsDom.all._
import autowire._

import querki.api._
import querki.display.{ButtonGadget, QText}
import querki.display.rx.GadgetRef
import querki.globals._
import querki.imexport.ImexportFunctions

class AdvancedPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  lazy val thingId = TID(params("thingId"))
  
  lazy val Client = interface[querki.client.Client]
  
  val ql =
    s"""""### Advanced commands for ____
        |
        |**[Export all Instances of [[Name]] as a CSV file](_exportModel?modelId=[[_oid]]&format=1)**
        |
        |[[_if(_hasPermission(Can Read Comments._self), 
        |  ""**Send me a Message whenever someone comments in this Space:** [[_space -> _getCommentNotifications._edit]]
        |("Maybe" means the default: Yes if you are the owner of this space, No otherwise.)
        |**Note:** this may not take effect for a few hours."")]]""""".stripMargin
        
  val exportedXML = GadgetRef.of[dom.html.Div]

  def pageContent = for {
    gutsRaw <- Client[ThingFunctions].evaluateQL(thingId, ql).call()
    guts = 
      div(
        new QText(gutsRaw),
        if (DataAccess.request.isOwner) {
          div(
            p("Press this button to export this *entire* Space as XML:"),
            new ButtonGadget(ButtonGadget.Normal, "Export Space to XML")({ () =>
              Client[ImexportFunctions].exportSpace().call().foreach { xml =>
                exportedXML <= div(pre(xml))
              }
            }),
            exportedXML <= div()
          )
        },
        new ButtonGadget(ButtonGadget.Primary, "Done")({ () => Pages.showSpacePage(DataAccess.space.get) })
      )
  }
    yield PageContents(s"Advanced Commands", guts)
}
