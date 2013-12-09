package querki.spaces

import models.{Property, PType, PTypeBuilder, Thing}
import models.Thing.PropMap
import querki.values._

case class TypeChangeInfo(typeChanged:Boolean, newType:PType[Any] with PTypeBuilder[Any, Any], serializedValues:Map[Thing, String])

/**
 * This utility deals with the important edge case of trying to change the Type of a
 * Property. This is a sufficient big and complex problem as to demand its own class.
 */
object PropTypeMigrator {
  def prepChange(state:SpaceState, prop:Property[_,_], newProps:PropMap):TypeChangeInfo = {
    
  }
}