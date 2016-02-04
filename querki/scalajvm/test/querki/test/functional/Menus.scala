package querki.test.functional

// This file should contain all of the descriptions of the MenuBar and its contents

sealed case class Menu(id:String)

sealed case class MenuItem(id:String, menu:Menu)

object ActionsMenu extends Menu("_actionsMenu")
object DesignModelItem extends MenuItem("designAModel", ActionsMenu)
object CreateThingItem extends MenuItem("_createAnyThing", ActionsMenu)

trait FuncMenu { this:FuncMixin =>
  /**
   * Chooses the specified item, by first opening the menu and then clicking on it.
   */
  def clickMenuItem(item:MenuItem) = {
    click on item.menu.id
    waitFor(item.id)
    click on item.id
  }
}
