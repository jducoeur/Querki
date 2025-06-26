package querki.display.input

import querki.data.ThingInfo

trait ForProp { self: InputGadget[_] =>
  lazy val Editing = interface[querki.editing.Editing]

  val prop: ThingInfo

  override def path = Editing.propPath(prop.oid)
}
