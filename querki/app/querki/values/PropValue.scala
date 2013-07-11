package querki.values

import models._

/**
 * The central representation of a value of a property.
 * 
 * TODO: this does not inherently know the PType of the Property, nor the VT. That's a pain in the
 * butt. Can we fix it without tying ourselves in knots? Note that we *CANNOT* have this include the
 * Property itself -- this is used for intermediate values that aren't actually from Properties --
 * but it could incorporate the actual PType.
 */
trait PropValue extends TypedValue {
}
