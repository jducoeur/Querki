package querki.history

import querki.spaces.SpaceMessagePersistence.DHDeleteThing
import querki.ql.QLExp
import querki.values.{QLContext, QValue, RequestContext, SpaceState}
import querki.globals._

trait FindDeleted extends HistoryFolding with EcologyMember {

  private lazy val Core = interface[querki.core.Core]
  private lazy val QL = interface[querki.ql.QL]

  private lazy val YesNoType = Core.YesNoType
  private lazy val ExactlyOne = Core.ExactlyOne
  private lazy val LinkType = Core.LinkType

  // Copied from YesNoUtils
  // TODO: refactor YesNoUtils so that it doesn't require being mixed in with an Ecot. The challenge is
  // that the Ecot definition of Core isn't precisely the same as the definition here, because it is an
  // initRequires -- it *acts* the same, but it's a different type. We we need a concept of "a way to get
  // at Core".
  def toBoolean(typed: QValue): Boolean = {
    if (typed.pType == YesNoType)
      typed.firstAs(YesNoType).getOrElse(false)
    else
      false
  }

  def findAllDeleted(
    rc: RequestContext,
    predicateOpt: Option[QLExp],
    renderOpt: Option[QLExp]
  ): Future[List[QValue]] = {
    // This is a bit complex. Ultimately, we're building up a list of QValues, which are the rendered results
    // for each deleted item. But we need to track the OID for each of those, so that we can filter out duplicates.
    // And we need to keep track of the *previous* SpaceState before each deletion event, so that we can render
    // the deleted item in terms of that previous state.
    // Down at the bottom, we will throw away everything but the QValues.
    foldOverHistory((List.empty[(QValue, OID)], emptySpace)) { (v, record) =>
      val (soFar, prevState) = v
      val HistoryEvent(sequenceNr, evt) = record
      val state = evolveState(Some(prevState))(evt)
      evt match {
        case DHDeleteThing(req, thingId, modTime) => {
          // Evaluate the predicate on this thing in the context of the *previous* state, when the
          // Thing still existed:
          val endTime = ecology.api[querki.time.TimeProvider].qlEndTime
          val context = QLContext(ExactlyOne(LinkType(thingId)), Some(rc), endTime)(prevState, ecology)
          val predicateResultFut: Future[Boolean] = predicateOpt match {
            case Some(predicate) =>
              try {
                QL.processExp(context, predicate).map(_.value).map(toBoolean(_)).recover {
                  case ex: Exception => { ex.printStackTrace(); false }
                }
              } catch {
                case ex: Exception => Future.successful(false)
              }
            case _ => Future.successful(true)
          }
          predicateResultFut.flatMap { passes =>
            def justTheOID(): Future[(List[(QValue, OID)], SpaceState)] = {
              val deleted = (ExactlyOne(LinkType(thingId)), thingId)
              val filtered = soFar.filterNot(_._2 == thingId)
              Future.successful((deleted :: filtered, state))
            }

            if (passes) {
              // Passes the predicate, so figure out its name, and add it to the list:
              renderOpt.map { render =>
                prevState.anything(thingId).map { thing =>
                  QL.processExp(context, render).map { result =>
                    // Remove any *previous* deletions of this Thing -- we want to wind up with only the
                    // most recent. This is a little inefficient in Big-O terms, but I'm guessing that won't
                    // matter a lot in reality:
                    val deleted = (result.value, thingId)
                    val filtered = soFar.filterNot(_._2 == thingId)
                    (deleted :: filtered, state)
                  }.recoverWith {
                    case ex: Exception => {
                      ex.printStackTrace()
                      justTheOID()
                    }
                  }
                }.getOrElse {
                  // Oddly, we didn't find that OID in the previous state, so give up and show the raw OID:
                  justTheOID()
                }
              }.getOrElse {
                justTheOID()
              }
            } else {
              // Didn't pass the predicate, so just keep going:
              Future.successful((soFar, state))
            }
          }
        }

        // Anything other than deletions is ignored -- move along...
        case _ => Future.successful((soFar, state))
      }
    }
      .map { result =>
        val (deletedPairs, state) = result

        // Filter out all restored items
        val finalDeletedPairs =
          deletedPairs
            .filterNot { pair =>
              val (qv, oid) = pair
              state.anything(oid).isDefined
            }

        // Extract just the List of QValues from all of that:
        finalDeletedPairs.map(_._1)
      }
  }

}
