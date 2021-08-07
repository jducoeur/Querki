package querki.identity.skilllevel

import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._

import org.querki.jquery._
import org.querki.gadgets._

import querki.data._
import querki.display.{ButtonGadget, Dialog}
import querki.ecology._
import querki.globals._
import querki.session.UserFunctions

class SkillLevelEcot(e: Ecology) extends ClientEcot(e) with SkillLevel {
  def implements = Set(classOf[SkillLevel])

  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val StatusLine = interface[querki.display.StatusLine]
  lazy val UserAccess = interface[querki.identity.UserAccess]

  lazy val consts = DataAccess.std.skillLevel

  // Note that _current is a variable. This is so that we can change it at runtime without having to
  // reload the Client completely.
  var _current: Option[Complexity] = None

  def current: Complexity = {
    if (_current.isEmpty) {
      _current = Some(UserAccess.user.map(user => tid2Complexity(user.skillLevel)).getOrElse(EasyComplexity))
    }
    _current.get
  }
  def updateSkillLevel(): Unit = _current = None

  sealed case class ComplexityImpl(
    name: String,
    thing: ThingInfo,
    desc: String,
    includes: Set[Complexity],
    id: String
  ) extends Complexity {
    def selected = { current == this }

    def accepts(actual: Complexity): Boolean = {
      (actual == this) || (includes.contains(actual))
    }
  }

  lazy val EasyComplexity = ComplexityImpl(
    "Participant",
    consts.skillLevelEasy,
    """Appropriate for most people, who want to be able to participate in other peoples' Spaces and
      |create Spaces based on Apps, but don't want to design their own Spaces from scratch. In Participant Mode, Querki
      |keeps the complexity to a minimum while still giving you the tools you need to add and edit data, and
      |participate in conversations.""".stripMargin,
    Set(StandardComplexity, AdvancedComplexity),
    "_easyComplexity"
  )

  lazy val StandardComplexity = ComplexityImpl(
    "Builder",
    consts.skillLevelStandard,
    """For those who want to build Spaces that don't yet exist as Apps, tweak existing Apps to better suit
      |their needs, or manage their Spaces in more detail. Builder Mode adds the Model Designer, so that you
      |can define exactly the sort of data you need, as well as more powerful security tools.""".stripMargin,
    Set(AdvancedComplexity),
    "_standardComplexity"
  )

  lazy val AdvancedComplexity = ComplexityImpl(
    "Programmer",
    consts.skillLevelAdvanced,
    """For the Querki power user. Programmer Mode adds all the bells and whistles, so that you can customize
      |Spaces in more detail, write your own code in QL, design custom Types, and lots more.""".stripMargin,
    Set(),
    "_advancedComplexity"
  )

  lazy val levels = Seq(EasyComplexity, StandardComplexity, AdvancedComplexity)

  implicit def tid2Complexity(level: TID): Complexity = {
    levels.find(_.thing.oid == level).getOrElse(throw new Exception(s"Looking for unknown Complexity $level"))
  }

  def changeSkillLevel(): Unit = {
    // TODO: this is horrible. How can we better deal with the circular dependency between the Dialog and
    // the LevelGadgets?
    var _dialog: Dialog = null
    def select(level: ComplexityImpl) = {
      StatusLine.showUntilChange("Saving...")
      Client[UserFunctions].setComplexity(level.thing.oid).call().foreach { tid =>
        _dialog.done()
        _current = Some(tid2Complexity(tid))
        PageManager.reload().foreach { page =>
          StatusLine.showBriefly("Saved")
        }
      }
    }
    _dialog =
      new Dialog(
        "Choose Mode",
        div(
          p("""In what way would you prefer to use Querki? Please click on one of these options. If you aren't sure which to go for,
              |start with "Participant" -- you can always change it later.""".stripMargin),
          table(
            cls := "table table-hover",
            tbody(
              for (level <- levels)
                yield Gadget(
                  tr(
                    id := level.id,
                    if (level.selected) cls := "success",
                    td(b(level.name)),
                    td(level.desc)
                  ),
                  { e =>
                    $(e).click { evt: JQueryEventObject =>
                      select(level)
                    }
                  }
                )
            )
          ),
          p(
            """You can change this at any time, by clicking on your name in the upper right corner of the page.""".stripMargin
          )
        ),
        (ButtonGadget.Normal, Seq("Cancel", id := "_cancelSkillLevel"), { dialog => dialog.done() })
      )

    _dialog.show()
  }
}
