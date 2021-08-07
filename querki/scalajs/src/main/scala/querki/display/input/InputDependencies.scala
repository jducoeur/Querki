package querki.display.input

import querki.ecology._
import querki.globals._

trait InputDependencies {

  def register(
    gadget: DependentInputGadget,
    onPath: String
  ): Unit

  def unregister(
    gadget: DependentInputGadget,
    onPath: String
  ): Unit
  def updated(path: String): Unit
  def clear(): Unit
}

/**
 * This manages all of the dependencies between inputs on a single Page.
 */
class InputDependenciesHandler extends InputDependencies {

  def register(
    gadget: DependentInputGadget,
    onPath: String
  ) = {
    val ds: Set[DependentInputGadget] = dependencies.get(onPath).getOrElse(Set.empty)
    dependencies += (onPath -> (ds + gadget))
  }

  def unregister(
    gadget: DependentInputGadget,
    onPath: String
  ) = {
    val ds: Set[DependentInputGadget] = dependencies.get(onPath).getOrElse(Set.empty)
    dependencies += (onPath -> (ds - gadget))
  }

  def updated(path: String) = {
    dependencies.get(path).foreach { deps =>
      deps.foreach(_.dependencyUpdated())
    }
  }

  // Called by the Page when it unloads, to break any dependency cycles, so we don't
  // wind up with memory leaks:
  def clear() = dependencies = Map.empty

  private var dependencies = Map.empty[String, Set[DependentInputGadget]]
}
