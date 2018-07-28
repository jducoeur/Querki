package querki.security

import org.scalajs.dom.html

import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import rx._

import org.querki.gadgets._
import org.querki.jquery._

import querki.display.ButtonGadget
import querki.globals._

/**
 * Represents the concept of what to do when editing is finished.
 */
trait EditCompleter[T] {
  def editComplete(itemOpt: Option[T]): Unit
}

abstract class OneItemGadget[TItem](in: TItem)(implicit val ecology: Ecology, ctx: Ctx.Owner) 
  extends Gadget[html.Div] with EditCompleter[TItem]
{
  itemGadget =>

  //////////////////////
  //
  // Abstract Members
  //
  // Concrete classes must fill these in.
  //
    
  /**
   * Given one Item (in practice, the value currently being displayed), get the String to display
   * for it.
   */
  def displayName(current: TItem): String
  
  /**
   * Given one Item (in practice, the value currently being displayed), set up an Editor Gadget
   * for it. This is a Future[] specifically to allow you to do server communication, in order
   * to fetch more-detailed information.
   * 
   * @param current The Item to edit.
   * @param completer The object that will be called when editing is finished.
   * @return The Edit Gadget, ready to insert into the page.
   */
  def prepToEdit(current: TItem, completer: EditCompleter[TItem]): Future[Gadget[html.Div]]
  
  ////////////////////
    
  val v = Var(in)
  val itemDiv = GadgetRef.of[html.Div]
  
  class ItemDisplayGadget() extends Gadget[html.Anchor] {
    override def onCreate(e: html.Anchor) = {
      $(e).click { evt: JQueryEventObject =>
        prepToEdit(v.now, itemGadget).map { panel: Gadget[html.Div] =>
          itemDiv <= panel
        }
        evt.preventDefault()
      }      
    }
    
    def doRender() = a(href := "#", displayName(v.now))
  }
  
  def displayItemName() = 
    itemDiv <= div(
      new ItemDisplayGadget()
    )
  
  def editComplete(newItemOpt: Option[TItem]) = {
    newItemOpt.map(newItem => v() = newItem)
    displayItemName()
  }
  
  def doRender() =
    div(
      displayItemName()
    )
}

/**
 * Shows a List of Items, each of which can be clicked on to edit it. Includes a Create button for adding another one.
 */
abstract class ItemListManager[TItem](itemList: Seq[TItem], title: String, buttonLabel: String, details: TypedTag[_])
  (implicit val ecology: Ecology, ctx: Ctx.Owner)
  extends Gadget[html.Div] with EditCompleter[TItem]
{
  //////////////////////
  //
  // Abstract Members
  //
  // Concrete classes must fill these in.
  //

  /**
   * Display one Item, by creating an instance of a subclass of OneItemGadget for it.
   */
  def showItem(item: TItem): OneItemGadget[TItem]
  
  /**
   * Do whatever is necessary to set up an Editor, ready to create a new Item. This returns a
   * Future, specifically so that you may do server communication to fetch additional info.
   */
  def prepToCreate(completer: EditCompleter[TItem]): Future[Gadget[html.Div]]
  
  /////////////////////
  
  val addDiv = GadgetRef.of[html.Div]
  val itemDiv = GadgetRef.of[html.Div]
  val createButton = GadgetRef[ButtonGadget]
  
  def editComplete(newItemOpt: Option[TItem]) = {
    itemDiv <= div()
    createButton.mapElemNow($(_).show())
    newItemOpt.map { newItem =>
      addDiv.mapElemNow { e =>
        $(e).append(showItem(newItem).render)
      }
    }
  }
  
  def doRender() =
    div(
      h4(title),
      details,
      for {
        item <- itemList
      }
        yield showItem(item),
        
      // New Items will get placed in here:
      addDiv <= div(),
      // We stick the edit panel in here, when you add a new one:
      itemDiv <= div(),
      createButton <= new ButtonGadget(ButtonGadget.Warning, buttonLabel) ({ () =>
        prepToCreate(this).map { panel =>
          createButton.mapElemNow($(_).hide())
          itemDiv <= panel
        }
      })
    )
}
