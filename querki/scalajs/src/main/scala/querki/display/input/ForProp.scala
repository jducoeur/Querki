package querki.display.input

import querki.data.ThingInfo
import querki.globals._

trait ForProp { self: InputGadget[_] =>
  lazy val Editing = interface[querki.editing.Editing]
  
  val prop: ThingInfo
  
  override def path = Editing.propPath(prop.oid)
}
