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
import querki.identity.skilllevel.SkillLevelsNeeded
import querki.util.InputUtils
  
class CreateNewPropertyGadget(page:ModelDesignerPage, typeInfo:AllTypeInfo, apg:AddPropertyGadget)(implicit val ecology:Ecology) 
  extends Gadget[dom.HTMLDivElement] with SkillLevelsNeeded with EcologyMember 
{
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Editing = interface[Editing]
  
  val std = DataAccess.std
  
  def isAdvanced = userSkillLevel == Advanced
  
  lazy val preferredCollectionsByType:Map[TID, TID] = {
    val allTypes = typeInfo.standardTypes ++ typeInfo.advancedTypes
    val pairs = allTypes.map { typeInfo =>
      typeInfo.preferredCollection.map(collId => (typeInfo.oid, collId))
    }.flatten
    Map(pairs:_*)
  }
  
  def reset() = {
    nameInput.map(_.setValue(""))
    typeSelector.map(_.setValue(""))
    modelSelector.map(_.setValue(""))
    collSelector.map(_.choose(collButtons.head))
  }
  
  override def onInserted() = { nameInput.mapElem($(_).focus()) }
  
  val nameInput = GadgetRef[RxInput].
    whenSet { g =>
      Obs(g.elemOptRx) {
        g.elemOpt.map { e => $(e).blur { evt:JQueryEventObject => fixNameInput(g.text()) } }
      }
    }
  // When we blur out of the Name field, fix it up if necessary:
  def fixNameInput(init:String):Unit = {
    def fixRec(current:String):String = {
      val c = current.last
      // These are legal, but *not* at the end of the name:
      if (c == ' ' || c == '-')
        fixRec(current.dropRight(1))
      else
        current
    }
    
    val fixed = fixRec(init)
    if (fixed != init)
      nameInput.get.setValue(fixed)
  }
  
  // TODO: should the Collections simply come from the global info instead of typeInfo? They aren't changeable yet.
  def collBtn(coll:CollectionInfo, selected:Boolean):ButtonInfo = {
    ButtonInfo(coll.oid.underlying, coll.displayName, selected, id:=s"_coll${coll.oid.underlying}")
  }
  lazy val collButtons =
    typeInfo.collections.headOption.map { collBtn(_, true) } ++
    typeInfo.collections.tail.map { collBtn(_, false) }
  lazy val collSelector = GadgetRef[RxButtonGroup]
  
  def collButton(collection:TID):ButtonInfo = {
    collButtons.find(_.value == collection.underlying).get
  }

  // Note that Type and Model both register listeners so that, when the user sets one, it clears the other:
  val typeSelector:GadgetRef[RxSelect] = GadgetRef[RxSelect].
    whenSet { g => 
      Obs(g.selectedValOpt) {
        g.selectedValOpt().map{ selectedType =>
          // They've selected a Type, so reset the Model...
          modelSelector.map(_.setValue("")) 
          // ... and set the Collection to best suit this Type:
          collSelector.map { sel =>
            val selectedTID = TID(selectedType)
            preferredCollectionsByType.get(selectedTID) match {
              // There's a preferred option:
              case Some(collId) => sel.choose(collButton(collId))
              // If there is nothing preferred, use Optional:
              case _ => sel.choose(collButton(std.core.optionalColl))
            }
          }
        }
      } 
    }
  val modelSelector = GadgetRef[RxSelect].
    whenSet { g => 
      Obs(g.selectedValOpt) {
        g.selectedValOpt().map { _ =>
          // They've selected a Model, so reset the Type...
          typeSelector.map(_.setValue("")) 
          // ... and set the Collection to List. Yes, this is hardcoded. So far,
          // I have yet to see an example where you want any Collection *other*
          // than List when you're incorporating a Model value.
          // TODO: this isn't very Rx-ish. Can/should we make this more properly
          // declarative and reactive?
          collSelector.map { _.choose(collButton(std.core.listColl)) }
        }
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
              nameInput <= new RxInput(Some(InputUtils.nameFilter(true, false) _), "text", cls:="col-md-6 form-control", id:="_createPropName", placeholder:="Name (required)...")
            )
          ),
          div(cls:="row",
            div(cls:="col-md-5", 
              typeSelector <= RxSelect(
                Var({typeInfo.advancedTypes.sortBy(_.displayName).map(typ => option(value:=typ, typ.displayName))}), 
                "Choose a Type...", 
                id:="_typeSelector",
                cls:="form-control")), 
            if (isAdvanced)
              span(cls:="col-md-1", " or "), 
            div(cls:="col-md-5", 
              display:={if (isAdvanced) "inline" else "none"},
              modelSelector <= RxSelect(
                Var({typeInfo.models.sortBy(_.displayName).map(model => option(value:=model, model.displayName))}), 
                "Make it a List of...",
                id:="_modelSelector",
                cls:="form-control"))
          ),
          div(cls:="row",
            div(cls:="col-md-12",
              collSelector <= new RxButtonGroup(Var(collButtons.toSeq), id:="_collSelector")
            )
          ),
          p(cls:="col-md-offset1",
            addButton
          )
        ),
        div(cls:="col-md-6 _typeDescription", 
          new DescriptionDiv(page, selectedBasis)
        )
      ),
      hr,
      p(new ButtonGadget(Info, id:="_addExistingInstead", "Add an Existing Property")({ () => apg.mainDiv.get.replaceContents(apg.addExisting.rendered, true) }), apg.cancelButton)
    )
}
