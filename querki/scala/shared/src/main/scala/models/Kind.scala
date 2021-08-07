package models

/**
 * Enumeration of what sort of Thing this is. Note that this is an intentionally
 * exclusive set. That's mostly to make it reasonably easy to reason about stuff:
 * if something is a Type, that means it isn't ordinary.
 */
object Kind {
  type Kind = Int

  val Thing = 0
  val Type = 1
  val Property = 2
  val Space = 3
  val Collection = 4

  def fromName(name: String): Option[Kind] = {
    val lower = name.toLowerCase()
    lower match {
      case "thing"      => Some(Thing)
      case "type"       => Some(Type)
      case "property"   => Some(Property)
      case "space"      => Some(Space)
      case "collection" => Some(Collection)
      case _            => None
    }
  }

  def getName(kind: Kind): Option[String] = {
    kind match {
      case Thing      => Some("thing")
      case Type       => Some("type")
      case Property   => Some("property")
      case Space      => Some("space")
      case Collection => Some("collection")
      case _          => None
    }
  }
}
