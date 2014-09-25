package querki.comm
  
import scala.scalajs.js

/**
 * Provides access to the routing table.
 * 
 * All entry points used by the Client (which in practice probably means all of them) should be
 * declared in the clientRoutes table in client.scala.html. Twirl turns that into proper Javascript
 * calls, and exposes them to the global namespace as "clientRoutes". This, in turn, picks those up
 * and provides them to the Client code as needed.
 * 
 * @TODO: this isn't bad, but it's still mediocre -- since it's js.Dynamic, there's no static checking
 * of the calls at all. We'd like to do something that's truly strongly typed, but Autowire isn't the
 * solution to this particular problem, since it's fundamentally Play-based. The right solution is
 * probably at compile time, to build something that gets at the routing information *before* Scala.js,
 * reflects on that, and generates the client-side glue code.
 */
@scala.scalajs.js.annotation.JSName("clientRoutes")
object ClientRoutes extends js.Object {
  def controllers:js.Dynamic = ???
}
