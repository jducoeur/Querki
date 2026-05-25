package querki.editing

import scala.concurrent.Future

import upickle.default.{macroRW, ReadWriter => RW}

import models.Wikitext

import querki.api.OperationHandle
import querki.data._

trait EditFunctions {
  import EditFunctions._

  /**
   * The central "edit" action: change the presence or content of one Property on a Bundle.
   */
  def alterProperty(
    thingId: TID,
    change: PropertyChange
  ): Future[PropertyChangeResponse]

  /**
   * Create a new Thing with the given Model and properties.
   */
  def create(
    modelId: TID,
    initialProps: Seq[PropertyChange]
  ): Future[ThingInfo]

  /**
   * Fetch the Editor for the specified Property on this Thing. The Property does not have to
   * be defined on the Thing yet.
   */
  def getOnePropertyEditor(
    thingId: TID,
    propId: TID
  ): Future[PropEditInfo]

  /**
   * Fetch the Editors and ancillary information about this Thing.
   */
  def getPropertyEditors(thingId: TID): Future[FullEditInfo]

  /**
   * Add this Property to this Thing, and return the Editor for it.
   */
  def addPropertyAndGetEditor(
    thingId: TID,
    propId: TID
  ): Future[PropEditInfo]

  /**
   * Remove the specified Property from the specified Thing.
   */
  def removeProperty(
    thingId: TID,
    propId: TID
  ): Future[PropertyChangeResponse]

  /**
   * Fetch a Type based on this Model, creating it if necessary.
   */
  def getModelType(modelId: TID): Future[TypeInfo]

  /**
   * Changes the Model that this Thing is based on.
   */
  def changeModel(
    thingId: TID,
    newModel: TID
  ): Future[ThingInfo]

  /**
   * Fetch the Large Text value used to reify Tags for this Model.
   */
  def getUndefinedTagView(modelId: TID): String

  /**
   * Fetch the number of uses of this Property in this Space.
   *
   * TODO: this is necessarily local to the Space. How do we handle it when
   * editing an App?
   */
  def getPropertyUsage(propId: TID): PropUsage

  /**
   * LONG RUNNING PROCESS: remove the specified Property from all Things in this Space.
   *
   * This is a big and dangerous change, so the Client should confirm it with the user
   * before invoking this call.
   */
  def removePropertyFromAll(propId: TID): OperationHandle
}

object EditFunctions {
  sealed trait PropertyChange

  /**
   * Describes a changed value on a Property. Note that the values are a List, to be able to support
   * Optional, List and Set.
   */
  case class ChangePropertyValue(
    path: String,
    currentValues: Seq[String]
  ) extends PropertyChange

  case class MoveListItem(
    path: String,
    from: Int,
    to: Int
  ) extends PropertyChange

  case class AddListItem(path: String) extends PropertyChange

  case class DeleteListItem(
    path: String,
    index: Int
  ) extends PropertyChange

  case class AddToSet(
    path: String,
    value: String
  ) extends PropertyChange

  case class RemoveFromSet(
    path: String,
    value: String
  ) extends PropertyChange

  case class MultiplePropertyChanges(changes: Seq[PropertyChange]) extends PropertyChange

  object PropertyChange {
    implicit val cpvrw: RW[ChangePropertyValue] = macroRW
    implicit val mlirw: RW[MoveListItem] = macroRW
    implicit val alirw: RW[AddListItem] = macroRW
    implicit val dlirw: RW[DeleteListItem] = macroRW
    implicit val atsrw: RW[AddToSet] = macroRW
    implicit val rfsrw: RW[RemoveFromSet] = macroRW
    implicit val mpcrw: RW[MultiplePropertyChanges] = macroRW
    implicit val rw: RW[PropertyChange] = macroRW
  }

  sealed trait PropertyChangeResponse
  case object PropertyChanged extends PropertyChangeResponse
  // Client-only, used when we are not actually making changes now; never returned from the server.
  case object PropertyNotChangedYet extends PropertyChangeResponse

  object PropertyChangeResponse {
    implicit val pcrw: RW[PropertyChanged.type] = macroRW
    implicit val pncyrw: RW[PropertyNotChangedYet.type] = macroRW
    implicit val rw: RW[PropertyChangeResponse] = macroRW
  }

  case class FullEditInfo(
    instancePropIds: Seq[TID],
    instancePropPath: String,
    derivingName: Boolean,
    propInfos: Seq[PropEditInfo]
  )

  object FullEditInfo {
    implicit val rw: RW[FullEditInfo] = macroRW
  }

  case class PropEditInfo(
    propInfo: PropInfo,
    path: String,
    prompt: Option[Wikitext],
    tooltip: Option[Wikitext],
    inheritedFrom: Option[String],
    canEditProperty: Boolean,
    // This is the raw HTML for the Editor
    editor: String
  )

  object PropEditInfo {
    implicit val rw: RW[PropEditInfo] = macroRW
  }

  case class PropUsage(
    nModels: Int,
    nInstances: Int
  )

  object PropUsage {
    implicit val rw: RW[PropUsage] = macroRW
  }
}
