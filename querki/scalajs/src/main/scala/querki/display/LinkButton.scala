package querki.display

import scalatags.JsDom.all._

import querki.data.BasicThingInfo
import querki.globals._
import ButtonGadget._

object ThingLinkButton extends QuerkiUIUtils {

  /**
   * Convenience function for creating a quick link to a specific Thing:
   */
  def apply(
    kind: ButtonKind,
    thing: BasicThingInfo,
    mods: Modifier*
  ) =
    a(cls := s"btn $kind", href := thingUrl(thing), mods)
}

object SpaceLinkButton {

  def apply(
    kind: ButtonKind,
    mods: Modifier*
  )(implicit
    ecology: Ecology
  ) = {
    val space = ecology.api[querki.data.DataAccess].space.get
    ThingLinkButton(kind, space, mods: _*)
  }
}
