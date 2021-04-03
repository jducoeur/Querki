package querki.history

import akka.pattern._
import querki.ecology._
import querki.globals._
import querki.values.RequestContext
import HistoryFunctions._
import models.OID
import querki.core.QLText
import querki.history.SpaceHistory.{DeletedThings, ForAllDeletedThings, RestoreDeletedThing}
import querki.ql.{ParsedQLText, QLCall, QLExp, QLThingId}
import querki.spaces.messages.{SpaceSubsystemRequest, ThingFound}
import querki.util.{ActorHelpers, PublicException}

object MOIDs extends EcotIds(65) {
  val FindAllStompedCmdOID = moid(1)
  val HistoryPermOID = moid(2)
  val UndeleteFunctionOID = moid(3)
  val ShowDeletedThingsOID = moid(4)
}

class HistoryEcot(e: Ecology) extends QuerkiEcot(e) with History with querki.core.MethodDefs {
  import MOIDs._

  val cmds = new HistoryCommands(e)

  val AccessControl = initRequires[querki.security.AccessControl]

  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]

  implicit val timeout = ActorHelpers.timeout

  override def postInit() = {
    ApiRegistry
      .registerApiImplFor[HistoryFunctions, HistoryFunctionsImpl](SpaceOps.spaceRegion, allowedDuringHistory = true)
  }

  def viewingHistoryVersion(rc: RequestContext): Option[HistoryVersion] = {
    rc.rawParam("_historyVersion") match {
      case Some(vStr) =>
        Some(vStr.toLong)
      case _ => None
    }
  }

  def isViewingHistory(rc: RequestContext): Boolean = {
    rc.rawParam("_historyVersion").map(_.length > 0).getOrElse(false)
  }

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
        reqs = Seq(
          ("oid", LinkType, "The OID of the Thing to restore")
        ),
        opts = Seq.empty,
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
        exp <- inv.opt(expOpt, Some(PublicException("History.undeleteNoOid")))
        oid <- inv.opt(expToOid(exp), Some(PublicException("History.undeleteNotOid", exp.reconstructString)))
        ThingFound(id, newState) <- inv.fut(
          SpaceOps.spaceRegion ?
            SpaceSubsystemRequest(inv.context.request.requesterOrAnon, inv.state.id, RestoreDeletedThing(user, oid))
        )
      } yield ExactlyOne(LinkType(id))
    }
  }

  lazy val ShowDeletedThings = new InternalMethod(
    ShowDeletedThingsOID,
    toProps(
      setName("_showDeletedThings"),
      Summary("Shows the Things in this Space that pass the given predicate, that have been deleted."),
      Signature(
        expected = None,
        reqs = Seq.empty,
        opts = Seq(
          ("predicate", AnyType, Core.QNone, "A QL expression returning YesOrNoType, to try on each deleted Thing")
        ),
        returns = (TextType, "A List of Text entries for the deleted Things")
      ),
      Details("""If no predicate is given, all deleted Things will be returned.""")
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      for {
        rawOpt <- inv.rawParam("predicate")
        qlText = rawOpt.map(raw => QLText(raw.reconstructStandalone))
        DeletedThings(things) <- inv.fut(
          SpaceOps.spaceRegion ?
            SpaceSubsystemRequest(
              inv.context.request.requesterOrAnon,
              inv.state.id,
              ForAllDeletedThings(inv.context.request, qlText)
            )
        )
        thing <- inv.iter(things)
        displayStr =
          s"""${thing.display.str} (${thing.oid.toThingId})
             |-- deleted ${thing.when.toString("YYYY-MM-dd hh:mma")}
             |[[_QLButton(label=""Restore"", ql=_undeleteThing(${thing.oid.toThingId}), noIcon=true)]]""".stripMargin
        // TODO: create a more useful return display
      } yield ExactlyOne(TextType(displayStr))
    }
  }

  override lazy val props = Seq(
    HistoryPerm,
    UndeleteFunction,
    ShowDeletedThings
  )
}
