package querki.security

import scalatags.JsDom.all._
import rx._
import autowire._

import querki.api.ThingFunctions
import querki.display._
import querki.display.rx._
import RxEmptyable._
import querki.globals._
import querki.pages._

class MakeShareableLinkPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("makeShareable") {
  
  lazy val thingId = TID(params("thingId"))
  lazy val isLinkToSpace = thingId == DataAccess.spaceId
  
  lazy val Client = interface[querki.client.Client]
  
  val nameInput = GadgetRef[RxText]
  val permChoiceInput = GadgetRef[RxRadio]
  
  def pageContent = for {
    permChoices <- Client[SecurityFunctions].getLinkPermChoices().call()
    choiceMap = 
      permChoices
        .map { choice => (choice.name -> choice.perms) }
        .toMap
    choiceButtons = permChoices.map { choice =>
      RadioButton(choice.name, choice.name, false)
    }
    thingInfo <- Client[ThingFunctions].getThingInfo(thingId).call()
    guts = div(
      h1(s"Make a Shareable Link for ${thingInfo.displayName}"),
      QText(s"""This will give you a link that you can put in email, web pages, Facebook posts and so on. *Anyone*
               |who can see the link will be able to access ${thingInfo.displayName}, so use it with care!""".stripMargin),
      p("Give this Link a name:"),
      nameInput <= new RxText(cls:="form-control"),
      p("What should the users of this link be able to do?"),
      permChoiceInput <= new RxRadio("_choiceButtons", choiceButtons:_*),
      p(new ButtonGadget(ButtonGadget.Primary, "Get Shareable Link", 
          disabled := Rx { nameInput.rxEmpty() || permChoiceInput.rxEmpty() })(
       {() =>
        // TODO: call makeShareableLink(), display the result, and copy it to the clipboard
       }))
    )
  }
    yield PageContents(guts)
}
