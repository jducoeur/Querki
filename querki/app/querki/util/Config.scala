package querki.util

import play.api.{Configuration,Play}

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
  def getTyped[T](key:String, default:Seq[T], playFetcher:Configuration => Option[T], localParser:String => T):T = {
    val optV = Play.maybeApplication match {
      case Some(app) => playFetcher(app.configuration)
      case None => mocks.get(key).map(localParser(_))
    }
    optV.getOrElse {
      if (default.isEmpty)
        throw new Exception("Didn't find required config parameter " + key)
      else
        default.head
    }
  }
  
  def getInt(key:String, default:Int*):Int = getTyped(key, default, (_.getInt(key)), (_.toInt))
  def getString(key:String, default:String*) = getTyped(key, default, (_.getString(key)), (_.toString()))
  def getBoolean(key:String, default:Boolean*) = getTyped(key, default, (_.getBoolean(key)), (_.toBoolean))
  
  /***********************************
   * MOCKUPS FOR TESTING
   **********************************/
  
  private val mocks = scala.collection.mutable.Map.empty[String,String]
  
  def test(key:String, v:String) = {
    mocks += (key -> v)
  }
}