package querki.security

import rx._

import org.querki.gadgets._

import querki.data.ThingInfo
import querki.display.input.InputGadget
import querki.editing.EditFunctions._
import querki.globals._

/**
 * Base trait to easily create gadgets or other things that participate in SaveablePropertyValue.
 */
trait SaveableBase extends EcologyMember {
  lazy val Editing = interface[querki.editing.Editing]

  def prop: ThingInfo
  def values: List[String]

  def path = Editing.propPath(prop.oid)
  def propertyChangeMsg() = ChangePropertyValue(path, values)
}

/**
 * Base trait for hooking an Rx Gadget into Saveables. Note that you need to fill in the prop
 * from SaveableBase as well.
 */
trait RxSaveable[R] extends SaveableBase {
  val rx: Rx[R]
  def values = saveables(rx.now)
  def saveables(r: R): List[String]
}

case class SaveableRxBoolean(
  prop: ThingInfo,
  rx: Rx[Boolean]
)(implicit
  val ecology: Ecology
) extends RxSaveable[Boolean] {

  def saveables(r: Boolean) =
    if (r)
      List("on")
    else
      List("off")
}

/**
 * Simple saveable when we don't even want to display an Input, we just want a hardcoded value.
 */
case class HardcodedSaveable(
  prop: ThingInfo,
  values: List[String]
)(implicit
  val ecology: Ecology
) extends SaveableBase

/**
 * Typeclass representing something that contains a Property Value that we know how to Save.
 */
trait SaveablePropertyValue[T] {
  // Fetch the change represented here, if that currently makes sense.
  def get(saveable: T): Option[PropertyChange]
}

object SaveablePropertyValue {

  implicit class SaveableOps[S : SaveablePropertyValue](s: S) {
    def getSaveable: Option[PropertyChange] = implicitly[SaveablePropertyValue[S]].get(s)
  }

  implicit val saveableInputGadget = new SaveablePropertyValue[InputGadget[_]] {
    def get(g: InputGadget[_]) = Some(g.propertyChangeMsg())
  }

  implicit def saveableGadgetRef[S <: Gadget[_] : SaveablePropertyValue] = new SaveablePropertyValue[GadgetRef[S]] {
    def get(gr: GadgetRef[S]) = gr.mapNow(_.getSaveable).flatten
  }

  implicit def saveableFromBase[B <: SaveableBase] = new SaveablePropertyValue[B] {
    def get(h: B) = Some(h.propertyChangeMsg())
  }
}
