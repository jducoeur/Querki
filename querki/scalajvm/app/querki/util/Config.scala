package querki.util

import scala.concurrent.duration._

import play.api.{Configuration,Play}

import querki.ecology.PlayEcology
import querki.globals._

/**
 * Querki's Configuration-param system.
 * 
 * Why are we rolling our own system, when Play has a fine one built in? Mostly
 * because the Play system, used naively, crashes if you try to use it outside the
 * context of a running application. And I don't adore Play's Option-heavy approach:
 * I often want to use defaults.
 * 
 * So this wraps around Play.configuration. It's a fairly thin shell -- see Play for
 * most of the details. But it allows you to run outside an application, with the
 * workaround of injecting pre-canned config params.
 */
object Config {
  def getTyped[T](key:String, default:Seq[T], playFetcher:Configuration => Option[T], localParser:String => T)(implicit ecology:Ecology):T = {
    // Note that we now access the Play Application through the Ecology:
    val optV = PlayEcology.maybeApplication match {
      // Normal case: the Play application is running
      case Some(app) => playFetcher(app.configuration)
      case None =>
        initConfigHack match {
          // HACK: We're trying to access Config during Application setup, probably from an Ecot's constructor
          case Some(initConfigs) => playFetcher(initConfigs)
          // There's nothing, so let's see if the unit-test mocks are in place:
          case _ => mocks.get(key).map(localParser(_))
        }
    }
    optV.getOrElse {
      if (default.isEmpty)
        throw new Exception("Didn't find required config parameter " + key)
      else
        default.head
    }
  }
  
  def getInt(key:String, default:Int*)(implicit ecology:Ecology):Int = getTyped(key, default, (_.getInt(key)), (_.toInt))
  def getString(key:String, default:String*)(implicit ecology:Ecology) = getTyped(key, default, (_.getString(key)), (_.toString()))
  def getBoolean(key:String, default:Boolean*)(implicit ecology:Ecology) = getTyped(key, default, (_.getBoolean(key)), (_.toBoolean))
  def getDuration(key:String, default:FiniteDuration*)(implicit ecology:Ecology):FiniteDuration = getTyped(key, default, { config =>
    config.getMilliseconds(key).map(Duration(_, MILLISECONDS))
  }, { str =>
    throw new Exception("Config.getDuration can not yet handle local strings!")
  })
  
  /**
   * HORRIBLE HACK: in the Play 2.4 world, we have an initialization-order problem. We build the Ecology, and
   * thus create the Ecots, during QuerkiApplicationLoader.load(). Problem is, Play.current doesn't become valid
   * until *after* that is complete. So any Ecots that need access to Config during their creation (which a few
   * do) wind up calling getTyped() above, don't get a result, and crash out.
   * 
   * So this is a workaround, set early in the Loader so that Ecots can get at it. It is *not* a good solution,
   * but I'm not sure what the right answer is...
   */
  var initConfigHack:Option[Configuration] = None 
  
  /***********************************
   * MOCKUPS FOR TESTING
   **********************************/
  
  private val mocks = scala.collection.mutable.Map.empty[String,String]
  
  def test(key:String, v:String) = {
    mocks += (key -> v)
  }
}
