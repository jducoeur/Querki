package querki.test.functional

import models.PType

/**
 * @author jducoeur
 */
trait FuncTypes { this: FuncMixin =>

  /**
   * Enumeration of the PTypes that we can test.
   */
  trait TType {

    type TSetter

    def tid: TID
    def display: String

    def setValue(
      thing: TThing[_],
      prop: TProp[this.type],
      v: TSetter
    ): Unit

    def prepProp(prop: TProp[this.type])(state: State): Unit = {}

    def fixupProp(prop: TProp[this.type])(state: State): Unit = {}
  }

  abstract class TSystemType[T](pt: PType[T]) extends TType {
    def tid = pt.id
    def display = pt.displayName
  }

  trait StringSetter { this: TType =>
    type TSetter = String

    def setValue(
      thing: TThing[_],
      prop: TProp[this.type],
      v: String
    ): Unit = {
      // Don't just set textField.value -- we need to be sure to tab out of here. This is because,
      // in the TTagType, it's going to display the menu of options (or the "not found" box) until
      // we exit this field, and that box can block the button we need next:
      click.on(editorId(thing, prop))
      enter(s"""$v\u0009""")
    }
  }

  lazy val TTextType = new TSystemType(ICore.TextType) with StringSetter

  lazy val TLargeTextType = new TSystemType(ICore.LargeTextType) {
    type TSetter = String

    def setValue(
      thing: TThing[_],
      prop: TProp[this.type],
      v: TSetter
    ): Unit = {
      textArea(editorId(thing, prop)).value = v
    }
  }

  /**
   * Utility function for setting up Link and Tag Properties.
   */
  trait ModelRestrictions { this: TType =>

    override def prepProp(prop: TProp[this.type])(state: State): Unit = {
      // TODO: Figure out a way to test *creation* of a Model, using the New button
      // TODO: Ick! A var! Surely there's a better way to do this...
      var hasModelRestriction = false
      prop.extras.collect {
        case RestrictedToModel(modelProto) => {
          waitFor("_pointerKind-Existing")
          // Select the existing-model option:
          click.on("_pointerKind-Existing")
          // HACK: for the moment, the options in the selector use OIDs, not TIDs:
          val modelId = state.thing(modelProto).tid
          singleSel("_pointerModelSelector").value = modelId
          hasModelRestriction = true
        }
      }
      if (!hasModelRestriction) {
        // There's no Model Restriction, so we need to click on the Any button:
        waitFor("_pointerKind-Any")
        click.on("_pointerKind-Any")
      }
    }
  }

  /**
   * Represents a Tag
   */
  lazy val TTagType = new TSystemType(ITags.NewTagSetType) with StringSetter with ModelRestrictions

  lazy val TLinkType = new TSystemType(ICore.LinkType) with ModelRestrictions {
    type TSetter = TThing[_]

    def setValue(
      thing: TThing[_],
      prop: TProp[this.type],
      v: TSetter
    ): Unit = {
      textField(editorId(thing, prop)).value = v.display
    }
  }
}
