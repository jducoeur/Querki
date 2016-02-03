package querki.test.functional

import scala.io.Source

import play.api.Application

import org.querki.shocon._

/**
 * This is essentially copied from LocalizationEcot, which is arguably evil. We might make it
 * shared instead? Does it belong in the shocon library itself?
 */
case class Messages(name:String, msgs:ObjectValue) {
  def getPackage(pkgName:String):Messages = {
    msgs.vs.get(pkgName) match {
      case Some(child @ ObjectValue(_)) => Messages(pkgName, child)
      case _ => throw new Exception(s"In Messages package $name, failed to find subpackage $pkgName")
    }
  }
  
  def msg(msgName:String, params:(String, String)*):String = {
    msgs.vs.get(msgName) match {
      case Some(child @ SimpleValue(text)) => {
        (text /: params) { (current, param) =>
          val (k,v) = param
          current.replaceAllLiterally("$" + k, v)
        }        
      }
      case _ => throw new Exception(s"In Messages package $name, failed to find message $msgName")
    }
  }
}

/**
 * Rough cognate to LocalizationEcot on the Client side, this loads the clientStrings,
 * parses them, and makes them available for tests.
 * 
 * @author jducoeur
 */
class FuncMessages(app:Application) {
  private val text =  {
    val source = Source.createBufferedSource(
      app.resourceAsStream("public/messages/default/clientStrings").getOrElse(throw new Exception("Couldn't find clientStrings!")))
    val text = source.getLines().mkString("\n")
    source.close()
    text
  }
  
  private val hoconRoot = HoconParse(text)
  val messages = Messages("", hoconRoot)
}
