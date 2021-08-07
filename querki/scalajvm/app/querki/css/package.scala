package querki

import querki.globals._

import models.Thing

package object css {

  /**
   * Describes the style info for a given Thing.
   *
   * @param sheets The actual stylesheets, in the correct order to display them to cascade properly.
   * @param headers Any other style-related headers that should be shown to render this Thing.
   */
  case class StyleInfo(
    sheets: Seq[String],
    headers: Seq[String]
  ) {
    def +(other: StyleInfo) = StyleInfo(sheets ++ other.sheets, headers ++ other.headers)
  }

  trait Stylesheets extends EcologyInterface {
    def stylesheetsFor(thing: Thing)(implicit state: SpaceState): StyleInfo
  }
}
