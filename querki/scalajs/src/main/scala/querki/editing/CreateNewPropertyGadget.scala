package querki.editing

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import autowire._
import rx._

import querki.api.{EditFunctions}
import EditFunctions._
import querki.data.{TID => _TID, _}
import querki.display.{ButtonGadget, Gadget}
import ButtonGadget._
import querki.display.rx.{ButtonInfo, RxAttr, RxButtonGroup, RxSelect, RxText}
import querki.globals._
  
class CreateNewPropertyGadget(page:ModelDesignerPage, typeInfo:AllTypeInfo, apg:AddPropertyGadget)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[Editing]
  
  val stdThingFut = DataAccess.standardThings
  
  def reset() = {
    nameInput.setValue("")
    typeSelector.setValue("")
    modelSelector.setValue("")
  }
  
  lazy val nameInput = new RxText(cls:="col-md-6 form-control", placeholder:="Name (required)...")
  
  // TODO: should the Collections simply come from the global info instead of typeInfo? They aren't changeable yet.
  lazy val collButtons =
    typeInfo.collections.headOption.map { coll => ButtonInfo(coll.oid.underlying, coll.displayName, true) } ++
    typeInfo.collections.tail.map { coll => ButtonInfo(coll.oid.underlying, coll.displayName) }
  lazy val collSelector = new RxButtonGroup(Var(collButtons.toSeq))
  
  val advTypeOptions = Var({
    val typeOpts = typeInfo.advancedTypes.sortBy(_.displayName).map(typ => option(value:=typ, typ.displayName))
    option(value:="", "Choose a Type...") +: typeOpts
  })
  val typeSelector = new RxSelect(advTypeOptions, cls:="form-control")
  
  val modelOptions = Var({
    val modelOpts = typeInfo.models.sortBy(_.displayName).map(model => option(value:=model, model.displayName))
    option(value:="", "Base it on a Model...") +: modelOpts
  })
  val modelSelector = new RxSelect(modelOptions, cls:="form-control")

  // You choose *either* a Type or a Model; when you set one, we unset the other:
  val modelClearer = Obs(typeSelector.selectedValOpt) {
    typeSelector.selectedValOpt().map(_ => modelSelector.setValue(""))
  }
  val typeClearer = Obs(modelSelector.selectedValOpt) {
    modelSelector.selectedValOpt().map(_ => typeSelector.setValue(""))
  }
  
  // The chosen basis is *either* a Model or a Type. selected() combines the currently-chosen value and its
  // RxSelect:
  lazy val selectedBasis = Rx { modelSelector.selectedWithTID() orElse typeSelector.selectedWithTID() }
  lazy val selectedBasisDescription = new DescriptionDiv(page, selectedBasis)
  lazy val selectedBasisDescriptionDiv = selectedBasisDescription.descriptionDiv
  
  // The add button is only enabled when all fields are non-empty; when pressed, it tells the parent
  // page to add the Property:
  lazy val addButton = 
    new ButtonGadget(Info, 
        RxAttr("disabled", Rx{ nameInput.textOpt().isEmpty || collSelector.selectedTIDOpt().isEmpty || selectedBasis().isEmpty }), 
        "Create")({ () =>
      val name = nameInput.textOpt().get
      val coll = collSelector.selectedTIDOpt().get
      val (selector, oid) = selectedBasis().get
      if (selector == modelSelector) {
        // We're creating it based on a Model, so we need to get the Model Type. Note that this is async:
        Client[EditFunctions].getModelType(oid).call().foreach { typeInfo => createProperty(name, coll, typeInfo.oid) }
      } else {
        // We already have a Type
        createProperty(name, coll, oid)
      }
    })
    
  def createProperty(name:String, collId:TID, typeId:TID) = {
    // Technically, we have to wait for the StandardInfo to be available:
    stdThingFut.foreach { stdThings =>
      def mkPV(oid:BasicThingInfo, v:String) = {
        val path = Editing.propPath(oid)
        ChangePropertyValue(path, Seq(v))
      }
      val initProps = Seq(
        mkPV(stdThings.core.nameProp, name),
        mkPV(stdThings.core.collectionProp, collId.underlying),
        mkPV(stdThings.core.typeProp, typeId.underlying)
      )
      Client[EditFunctions].create(stdThings.core.urProp, initProps).call().foreach { propInfo =>
        page.addProperty(propInfo.oid, true)
        reset()
      }
    }
  }
  
  def doRender() =
    div(cls:="well container col-md-12",
      p("""Describe the new property to create, or press "Add an Existing Property" to use one that already exists."""),
      div(cls:="row",
        div(cls:="col-md-6",
          div(cls:="row",
            div(cls:="col-md-12",
              nameInput
            )
          ),
          div(cls:="row",
            div(cls:="col-md-12",
              collSelector
            )
          ),
          div(cls:="row",
            div(cls:="col-md-5", typeSelector), span(cls:="col-md-1", " or "), div(cls:="col-md-5", modelSelector)
          )
        ),
        div(cls:="col-md-6", selectedBasisDescriptionDiv)
      ),
      p(cls:="col-md-offset1",
        addButton
      ),
      hr,
      p(new ButtonGadget(Info, "Add an Existing Property")({ () => apg.mainDiv.replaceContents(apg.addExisting.rendered, true) }), apg.cancelButton)
    )
}
