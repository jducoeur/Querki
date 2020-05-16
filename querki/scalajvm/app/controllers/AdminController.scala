package controllers

import javax.inject.{Inject, Provider}

import scala.concurrent.duration._
import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import models.{Thing, OID}
import play.api.mvc.{AnyContent, BodyParser, BodyParsers, EssentialAction, Result}
import play.twirl.api.Html
import querki.globals.Future
import querki.history.SpaceHistory
import querki.spaces.messages.CurrentState
import querki.util.QLog

class AdminController @Inject() (val appProv:Provider[play.api.Application]) extends ApplicationBase  {
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
   * Dumps a human-readable representation of the Space. Mainly intended for forensics, when a Space is broken.
   *
   * Note that this requires the OID of the Space.
   */
  def dumpSpaceReadable(spaceIdStr:String): EssentialAction = withAdmin { prc =>
    OID.parseOpt(spaceIdStr).map { spaceId =>
      // This is unusual and nasty. We boot up a special-purpose, short-lifespan Actor just for this, rather than
      // loading it conventionally.
      val app = appProv.get()
      val system = app.actorSystem
      val props = SpaceHistory.actorProps(ecology, spaceId, ActorRef.noSender)
      val historyRef = system.actorOf(props, s"spacereadable-$spaceIdStr")
      implicit val ec = system.dispatcher

      implicit val timeout = Timeout(60.seconds)
      (historyRef ? SpaceHistory.GetCurrentState(prc)).map {
        case CurrentState(state, _) => {
          // This Actor is one-and-done -- close it so we don't leak it:
          system.stop(historyRef)

          // Okay, we now have the SpaceState. Now we iterate over it, and print it all out.
          implicit val s = state
          def render(t: Thing): String = QLog.renderThing(t)
          def renderMap[T <: Thing](m: Map[OID, T]): String = {
            m.values.map(render(_)).mkString("<pre>\n", "\n\n", "</pre>\n")
          }

          val page =
            s"""
               |<html>
               |<body>
               |<h1>Dump of ${state.displayName}, version ${state.version.v}</h1>
               |
               |<h2>Space Info</h2>
               |
               |<pre>
               |${QLog.renderThing(state)}
               |
               |<b>Owner:</b> ${state.ownerIdentity}
               |</pre>
               |
               |<h2>Types<h2>
               |
               |${renderMap(state.types)}
               |
               |<h2>Properties</h2>
               |
               |${renderMap(state.spaceProps)}
               |
               |<h2>Things</h2>
               |
               |${renderMap(state.things)}
               |</body>
               |</html>
               |""".stripMargin

          Ok(new Html(page))
        }
        case _ => {
          BadRequest
        }
      }
    }.getOrElse(Future.successful(BadRequest))
  }
}
