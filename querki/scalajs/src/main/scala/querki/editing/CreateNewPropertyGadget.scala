package querki.editing

import org.scalajs.dom.{raw => dom}
import org.querki.jquery._
import scalatags.JsDom.all._
import autowire._
import rx._

import EditFunctions._
import querki.data.{TID => _TID, _}
import querki.display.{ButtonGadget, Gadget}
import ButtonGadget._
import querki.display.rx._
import querki.globals._
  
class CreateNewPropertyGadget(page:ModelDesignerPage, typeInfo:AllTypeInfo, apg:AddPropertyGadget)(implicit val ecology:Ecology) extends Gadget[dom.HTMLDivElement] with EcologyMember {

  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[Editing]
  
  val std = DataAccess.std
  
  def reset() = {
    nameInput.map(_.setValue(""))
    typeSelector.map(_.setValue(""))
    modelSelector.map(_.setValue(""))
    collSelector.map(_.choose(collButtons.head))
  }
  
  override def onInserted() = { nameInput.mapElem($(_).focus()) }
  
  val nameInput = GadgetRef[RxText]
  
  // TODO: should the Collections simply come from the global info instead of typeInfo? They aren't changeable yet.
  def collBtn(coll:CollectionInfo, selected:Boolean):ButtonInfo = {
    ButtonInfo(coll.oid.underlying, coll.displayName, selected, id:=s"_coll${coll.oid.underlying}")
  }
  lazy val collButtons =
    typeInfo.collections.headOption.map { collBtn(_, true) } ++
    typeInfo.collections.tail.map { collBtn(_, false) }
  lazy val collSelector = GadgetRef[RxButtonGroup]

  // Note that Type and Model both register listeners so that, when the user sets one, it clears the other:
  val typeSelector:GadgetRef[RxSelect] = GadgetRef[RxSelect].
    whenSet { g => 
      Obs(g.selectedValOpt) {
        g.selectedValOpt().map(_ => modelSelector.map(_.setValue("")))
      } 
    }
  val modelSelector = GadgetRef[RxSelect].
    whenSet { g => 
      Obs(g.selectedValOpt) {
        g.selectedValOpt().map(_ => typeSelector.map(_.setValue("")))
      }
    }
  
  // The chosen basis is *either* a Model or a Type. selected() combines the currently-chosen value and its
  // RxSelect:
  lazy val selectedBasis = Rx { modelSelector.flatMap(_.selectedWithTID()) orElse typeSelector.flatMap(_.selectedWithTID()) }
  
  // The add button is only enabled when all fields are non-empty; when pressed, it tells the parent
  // page to add the Property:
  lazy val addButton = 
    new ButtonGadget(Info, 
        disabled := Rx{ nameInput.get.textOpt().isEmpty || collSelector.get.selectedTIDOpt().isEmpty || selectedBasis().isEmpty }, 
        "Create", id:="_doCreatePropertyButton")({ () =>
      val name = nameInput.get.textOpt().get
      val coll = collSelector.get.selectedTIDOpt().get
      val (selector, oid) = selectedBasis().get
      if (selector == modelSelector.get) {
        // We're creating it based on a Model, so we need to get the Model Type. Note that this is async:
        Client[EditFunctions].getModelType(oid).call().foreach { typeInfo => createProperty(name, coll, typeInfo.oid) }
      } else {
        // We already have a Type
        createProperty(name, coll, oid)
      }
    })
    
  def createProperty(name:String, collId:TID, typeId:TID) = {
    def mkPV(oid:BasicThingInfo, v:String) = {
      val path = Editing.propPath(oid)
      ChangePropertyValue(path, Seq(v))
    }
    val initProps = Seq(
      mkPV(std.core.nameProp, name),
      mkPV(std.core.collectionProp, collId.underlying),
      mkPV(std.core.typeProp, typeId.underlying)
    )
    Client[EditFunctions].create(std.core.urProp, initProps).call().foreach { propInfo =>
      page.addProperty(propInfo.oid, true)
      reset()
      apg.reset()
    }
  }
  
  def doRender() =
    div(cls:="well container col-md-12",
      p("""Describe the new property to create, or press "Add an Existing Property" to use one that already exists."""),
      div(cls:="row",
        div(cls:="col-md-6",
          div(cls:="row",
            div(cls:="col-md-12",
              nameInput <= new RxText(cls:="col-md-6 form-control", id:="_createPropName", placeholder:="Name (required)...")
            )
          ),
          div(cls:="row",
            div(cls:="col-md-12",
              collSelector <= new RxButtonGroup(Var(collButtons.toSeq), id:="_collSelector")
            )
          ),
          div(cls:="row",
            div(cls:="col-md-5", 
              typeSelector <= RxSelect(
                Var({typeInfo.advancedTypes.sortBy(_.displayName).map(typ => option(value:=typ, typ.displayName))}), 
                "Choose a Type...", 
                id:="_typeSelector",
                cls:="form-control")), 
            span(cls:="col-md-1", " or "), 
            div(cls:="col-md-5", 
              modelSelector <= RxSelect(
                Var({typeInfo.models.sortBy(_.displayName).map(model => option(value:=model, model.displayName))}), 
                "Base it on a Model...",
                id:="_modelSelector",
                cls:="form-control"))
          )
        ),
        div(cls:="col-md-6", 
          new DescriptionDiv(page, selectedBasis)
        )
      ),
      p(cls:="col-md-offset1",
        addButton
      ),
      hr,
      p(new ButtonGadget(Info, id:="_addExistingInstead", "Add an Existing Property")({ () => apg.mainDiv.get.replaceContents(apg.addExisting.rendered, true) }), apg.cancelButton)
    )
}
