package querki.html

/**
 * Represents HTML content that has already been neutered, and is now safe to add to the page.
 */
case class QHtml(content:String) {
  override def toString() = content
}
