package querki.history

import akka.pattern._
import querki.ecology._
import querki.globals._
import querki.values.RequestContext
import HistoryFunctions._
import models.OID
import querki.history.SpaceHistory.{DeletedThings, ForAllDeletedThings, RestoreDeletedThing}
import querki.ql.{QLCall, QLExp, QLThingId}
import querki.spaces.messages.{SpaceSubsystemRequest, ThingFound}
import querki.util.{ActorHelpers, PublicException}

object MOIDs extends EcotIds(65) {
  // Dead -- we've removed this command, since we haven't used it in years:
//  val FindAllStompedCmdOID = moid(1)
  val HistoryPermOID = moid(2)
  val UndeleteFunctionOID = moid(3)
  val ListDeletedThingsOID = moid(4)
  val DeletedThingsPageOID = moid(5)
  val DeletedThingsDisplayOID = moid(6)
}

class HistoryEcot(e: Ecology) extends QuerkiEcot(e) with History with querki.core.MethodDefs {
  import MOIDs._

  val cmds = new HistoryCommands(e)

  val AccessControl = initRequires[querki.security.AccessControl]
  val Basic = initRequires[querki.basic.Basic]

  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]

  implicit val timeout = ActorHelpers.timeout

  override def postInit() = {
    ApiRegistry
      .registerApiImplFor[HistoryFunctions, HistoryFunctionsImpl](SpaceOps.spaceRegion, allowedDuringHistory = true)
  }

  def viewingHistoryVersion(rc: RequestContext): Option[HistoryVersion] = {
    rc.rawParam(HistoryFunctions.viewingHistoryParam) match {
      case Some(vStr) =>
        Some(vStr.toLong)
      case _ => None
    }
  }

  def isViewingHistory(rc: RequestContext): Boolean = {
    rc.rawParam(HistoryFunctions.viewingHistoryParam).map(_.length > 0).getOrElse(false)
  }

  ///////////////////////////////////
  // THINGS
  ///////////////////////////////////

  lazy val deletedThingsDisplay = new ThingState(
    DeletedThingsDisplayOID,
    systemOID,
    RootOID,
    toProps(
      setName("_deletedThingsDisplay"),
      Summary("The standard display of the deleted Things in this Space"),
      Basic.ApplyMethod(
        """_listDeletedThings(
          |   render=""[[Name]] ([[_oid]]) [[_QLButton(label = ""Restore"", noIcon = true, ql = _undeleteThing -> System Deleted Things Page -> _navigateTo)]]""
          |) -> _bulleted""".stripMargin
      )
    )
  )

  lazy val deletedThingsPage = new ThingState(
    DeletedThingsPageOID,
    systemOID,
    RootOID,
    toProps(
      setName("System Deleted Things Page"),
      Basic.DisplayTextProp("[[_deletedThingsDisplay]]")
    )
  )

  override lazy val things = Seq(
    deletedThingsDisplay,
    deletedThingsPage
  )

  ///////////////////////////////////
  // PROPERTIES
  ///////////////////////////////////

  lazy val HistoryPerm = AccessControl.definePermission(
    HistoryPermOID,
    "Can View History",
    "This permission controls whether someone is allowed to examine the History of this Space. This gives full read permission!",
    Seq(AccessControl.OwnerTag),
    Seq(AccessControl.AppliesToSpace),
    false,
    false
  )

  ///////////////////////////////////
  // FUNCTIONS
  ///////////////////////////////////

  lazy val UndeleteFunction = new InternalMethod(
    UndeleteFunctionOID,
    toProps(
      setName("_undeleteThing"),
      Summary("Given the OID of a deleted Thing, restore it"),
      Signature(
        expected = None,
        reqs = Seq.empty,
        opts = Seq(
          ("oid", LinkType, Core.QNone, "The OID of the Thing to restore")
        ),
        returns = (LinkType, "The restored Thing")
      ),
      Details(
        """Note that the OID is a parameter rather than a received value, and it has to be a literal OID.
          |That's necessarily true (you can't pass in the OID, since the Thing doesn't exist any more).
          |
          |This is primarily intended for internal use, from the Deleted Things page.
          |""".stripMargin
      )
    )
  ) {

    // Since the Thing presumably doesn't exist in this Space currently, it would be illegal to process it in
    // the normal way -- we would get a Thing Not Found error. Instead, we have to grovel down through the
    // raw parse tree to fetch the OID itself:
    def expToOid(exp: QLExp): Option[OID] = {
      for {
        phrase <- exp.phrases.headOption
        op <- phrase.ops.headOption
        call <- op match {
          case call: QLCall => Some(call)
          case _            => None
        }
        thingIdStr <- call.name match {
          case QLThingId(n) => Some(n)
          case _            => None
        }
        oid <- OID.parseOpt(thingIdStr)
      } yield oid
    }

    override def qlApply(inv: Invocation): QFut = {
      val user = inv.context.request.requesterOrAnon
      for {
        expOpt <- inv.rawParam("oid")
        oid <- expOpt match {
          case Some(exp) =>
            inv.opt(expToOid(exp), Some(PublicException("History.undeleteNotOid", exp.reconstructString)))
          case _ => inv.contextAllAs(LinkType)
        }
        ThingFound(id, newState) <- inv.fut(
          SpaceOps.spaceRegion ?
            SpaceSubsystemRequest(inv.context.request.requesterOrAnon, inv.state.id, RestoreDeletedThing(user, oid))
        )
      } yield ExactlyOne(LinkType(id))
    }
  }

  lazy val ListDeletedThings = new InternalMethod(
    ListDeletedThingsOID,
    toProps(
      setName("_listDeletedThings"),
      Summary("Lists the Things in this Space that pass the given filter, that have been deleted."),
      Signature(
        expected = None,
        reqs = Seq.empty,
        opts = Seq(
          ("filter", AnyType, Core.QNone, "A QL expression returning YesOrNoType, to try on each deleted Thing"),
          ("render", AnyType, Core.QNone, "A QL expression saying what to return from each deleted Thing")
        ),
        returns = (AnyType, "A List of resulting values from the deleting Things")
      ),
      Details(
        """
          |If no filter is given, all deleted Things will be returned.
          |
          |The filter can be almost anything. For example, this returns all deleted Things that are children of
          |`My Model`:
          |[[```
          |_listDeletedThings(filter = _model -> _is(My Model))
          |```]]
          |
          |The `render` parameter will be applied to each deleted Thing, in the context of the history right before
          |that Thing was deleted. You can return any sort of value here, both text and non-text. For example, this
          |renders all of the instances of `My Model`, showing the name and OID of each with "Restore" button to
          |undelete it:
          |[[```
          |_listDeletedThings(filter = _model -> _is(My Model), render=""[[Name]] ([[_oid]]) [[_QLButton(label = ""Restore"", noIcon = true, ql = _undeleteThing)]]"") -> _bulleted
          |```]]
          |
          |If no `render` parameter is given, the OIDs of the deleted Things will be returned.""".stripMargin
      )
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      for {
        predOpt <- inv.rawParam("filter")
        renderOpt <- inv.rawParam("render")
        DeletedThings(qvs) <- inv.fut(
          SpaceOps.spaceRegion ?
            SpaceSubsystemRequest(
              inv.context.request.requesterOrAnon,
              inv.state.id,
              ForAllDeletedThings(inv.context.request, predOpt, renderOpt)
            )
        )
        qv <- inv.iter(qvs)
      } yield qv
    }
  }

  override lazy val props = Seq(
    HistoryPerm,
    UndeleteFunction,
    ListDeletedThings
  )
}
