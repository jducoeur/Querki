package controllers

import java.time.ZoneId
import java.time.format.DateTimeFormatter

import javax.inject.{Inject, Provider}

import scala.concurrent.duration._
import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import models.{Kind, Thing, OID, ThingId}
import play.api.mvc.{AnyContent, BodyParser, BodyParsers, EssentialAction, Result}
import play.twirl.api.Html
import querki.data.TID
import querki.globals.Future
import querki.history.HistoryFunctions._
import querki.history.{HistoryFunctions, SpaceHistory}
import querki.spaces.messages.CurrentState
import querki.util.QLog

class AdminController @Inject() (val appProv:Provider[play.api.Application]) extends ApplicationBase {

  lazy val System = interface[querki.system.System]

  /**
   * Extended version of [[withUser()]], enforcing that this entry point may only be called by a logged-in Admin.
   */
  def withAdmin(
    f: PlayRequestContextFull[AnyContent] => Future[Result]
  ): EssentialAction = withUser(true) { prc =>
    if (prc.requesterOrAnon.isAdmin) {
      f(prc)
    } else {
      Future.successful(BadRequest)
    }
  }

  /**
   * This encapsulates the notion of quickly booting up the history for a Space, grabbing some information from it,
   * and shutting it down again.
   */
  def fromSpaceHistory(
    spaceIdStr: String,
    msg: PlayRequestContextFull[AnyContent] => Any
  )(
    f: PartialFunction[Any, Result]
  ): EssentialAction = withAdmin { prc =>
    OID.parseOpt(spaceIdStr).map { spaceId =>
      // This is unusual and nasty. We boot up a special-purpose, short-lifespan Actor just for this, rather than
      // loading it conventionally.
      val app = appProv.get()
      val system = app.actorSystem
      val props = SpaceHistory.actorProps(ecology, spaceId, ActorRef.noSender)
      val historyRef = system.actorOf(props, s"spacereadable-$spaceIdStr")
      implicit val ec = system.dispatcher

      implicit val timeout = Timeout(60.seconds)
      (historyRef ? msg(prc)).map { response =>
        // This Actor is one-and-done -- close it so we don't leak it:
        system.stop(historyRef)

        if (f.isDefinedAt(response)) {
          f(response)
        } else {
          BadRequest
        }
      }
    }.getOrElse(Future.successful(BadRequest))
  }

  /**
   * Dumps a human-readable representation of the Space. Mainly intended for forensics, when a Space is broken.
   *
   * Note that this requires the OID of the Space.
   */
  def dumpSpaceReadable(spaceIdStr: String): EssentialAction =
    fromSpaceHistory(spaceIdStr, SpaceHistory.GetCurrentState(_))
  {
    case CurrentState(state, _) => {
      // Okay, we now have the SpaceState. Now we iterate over it, and print it all out.
      implicit val s = state

      def render(t: Thing): String = QLog.renderThing(t)

      def renderMap[T <: Thing](m: Map[OID, T]): String = {
        m.values.map(render(_)).mkString("\n\n")
      }

      val page =
        s"""
           |# Dump of ${state.displayName}, version ${state.version.v}
           |
           |## Space Info
           |
           |${QLog.renderThing(state)}
           |
           |**Owner:** ${state.ownerIdentity}
           |
           |## Types
           |
           |${renderMap(state.types)}
           |
           |## Properties
           |
           |${renderMap(state.spaceProps)}
           |
           |Things
           |
           |${renderMap(state.things)}
           |""".stripMargin

      Ok(page)
    }
  }

  def dumpSpaceSummary(spaceIdStr: String): EssentialAction =
    fromSpaceHistory(spaceIdStr, SpaceHistory.GetHistorySummary(_))
  {
    case HistorySummary(events, context) => {
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault())

      def identities: String = {
        context.whoMap.map { case (oid, identityInfo) =>
          s"* $oid -> $identityInfo"
        }.mkString("\n")
      }

      def thingNames: String = {
        context.thingNames.map { case (tid, name) =>
          s"* $tid -> $name"
        }.mkString("\n")
      }

      def name(id: TID): String = {
        context
          .thingNames
          .get(id)
          .orElse {
            val oid = ThingId(id.underlying)
            System.State.thing(oid).map(_.displayName)
          }
          .getOrElse(id.toString)
      }

      def displayEvent(evt: EvtSummary): String = {
        val who = context.whoMap.get(evt.who).map(_.name).getOrElse(s"Unknown Person ${evt.who}")
        val when = formatter.format(java.time.Instant.ofEpochMilli(evt.time))
        val what = evt match {
          case SetStateSummary(idx, who, time, reason, details) =>
            s"SetState($reason, $details)"
          case ImportSummary(idx, who, time) =>
            s"Import()"
          case CreateSummary(idx, who, time, kind, id, model, restored) =>
            s"Create(${Kind.getName(kind).getOrElse("UNKNOWN KIND")}, model=${name(model)}, id=${name(id)}, restored=$restored)"
          case ModifySummary(idx, who, time, id, props) =>
            s"Modify(${name(id)}, props=${props.map(name(_)).mkString("(", ", ", ")")})"
          case DeleteSummary(idx, who, time, id) =>
            s"Delete(${name(id)})"
          case AddAppSummary(idx, who, time, appId) =>
            s"AddApp($appId)"
        }

        s"$who ($when): $what"
      }

      val page =
        s"""
          |# Event History for $spaceIdStr
          |
          |## Events, in order
          |
          |${events.map(displayEvent(_)).mkString("\n")}
          |
          |## Context
          |
          |### IdentityMap
          |
          |$identities
          |
          |### ThingNames
          |
          |$thingNames
          |
          |""".stripMargin

      Ok(page)
    }
  }
}
