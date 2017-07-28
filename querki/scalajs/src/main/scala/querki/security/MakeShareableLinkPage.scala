package querki.security

import org.scalajs.dom.html

import scalatags.JsDom.all._
import rx._
import autowire._
import org.querki.gadgets._

import querki.api.ThingFunctions
import querki.display._
import querki.display.rx._
import RxEmptyable._
import querki.globals._
import querki.pages._

class MakeShareableLinkPage(params:ParamMap)(implicit val ecology:Ecology) extends Page("makeShareable") {
  
  lazy val Client = interface[querki.client.Client]
  
  val nameInput = GadgetRef[RxText]
  val permChoiceInput = GadgetRef[RxRadio]
  val showLinkSection = GadgetRef.of[html.Div]
  val showLinkText = GadgetRef[RxText]
  
  val sharedLink:Var[Option[String]] = Var(None)
  val sharedLinkAsText = Rx { sharedLink().getOrElse("") }
  
  val spaceName = DataAccess.space.get.displayName
  
  def pageContent = for {
    permChoices <- Client[SecurityFunctions].getLinkPermChoices().call()
    choiceMap = 
      permChoices
        .map { choice => (choice.name -> choice.perms) }
        .toMap
    choiceButtons = permChoices.map { choice =>
      RadioButton(choice.name, choice.name, false)
    }
    guts = div(
      h1(s"Make a Shareable Link for $spaceName"),
      QText(s"""This will give you a link that you can put in email, web pages, Facebook posts and so on. *Anyone*
               |who can see the link will be able to access $spaceName, so use it with care!""".stripMargin),
      p("Give this Link a name:"),
      nameInput <= new RxText(cls:="form-control"),
      p("What should the users of this link be able to do?"),
      permChoiceInput <= new RxRadio("_choiceButtons", choiceButtons:_*),
      p(new ButtonGadget(ButtonGadget.Primary, "Get Shareable Link", 
          disabled := Rx {
            val nameEmpty = nameInput.rxEmpty
            val permChoiceEmpty = permChoiceInput.rxEmpty
            nameEmpty() || permChoiceEmpty() 
          })(
       {() =>
         Client[SecurityFunctions].makeShareableLink(nameInput.get.text.now, choiceMap(permChoiceInput.get.selectedVal.now)).call().map { link =>
           sharedLink() = Some(link)
         }
       })),
      showLinkSection <= div(display := Rx { if (sharedLink().isEmpty) "none" else "block" },
        p(b("Here is the link:")),
        p(new RxTextFrag(sharedLinkAsText))
      )
    )
  }
    yield PageContents(guts)
}
