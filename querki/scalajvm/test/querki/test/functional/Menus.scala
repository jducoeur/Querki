package querki.test.functional

// This file should contain all of the descriptions of the MenuBar and its contents

sealed case class Menu(id:String)

sealed case class MenuItem(id:String, menu:Menu)

object ActionsMenu extends Menu("_actionsMenu")
object RefreshItem extends MenuItem("_refreshMenuItem", ActionsMenu)
object DesignModelItem extends MenuItem("designAModel", ActionsMenu)
object CreateThingItem extends MenuItem("_createAnyThing", ActionsMenu)
object OpenAdvancedItem extends MenuItem("_openAdvancedItem", ActionsMenu)
object AdvancedEditItem extends MenuItem("_advEditButton", ActionsMenu)
object SecurityItem extends MenuItem("_securityItem", ActionsMenu)
object SharingItem extends MenuItem("_sharingButton", ActionsMenu)

object ProfileMenu extends Menu("_profile_menu")
object SkillLevelItem extends MenuItem("_skillLevelButton", ProfileMenu)
object LogoutItem extends MenuItem("logout_button", ProfileMenu)

trait FuncMenu { this:FuncMixin =>
  
  /**
   * Opens the menu that contains this item. You pass in an item *on* that menu so that
   * we have something to wait for to know that it's open.
   */
  def openMenuFor(item:MenuItem) = {
    waitFor(item.menu.id)
    click on item.menu.id
    waitFor(item.id)
  }
  
  /**
   * Chooses the specified item, by first opening the menu and then clicking on it.
   */
  def clickMenuItem(item:MenuItem) = {
    openMenuFor(item)
    click on item.id
  }
}
