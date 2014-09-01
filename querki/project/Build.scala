import sbt._
import Keys._
import play.Play._
//import scala.scalajs.sbtplugin.ScalaJSPlugin._
//import ScalaJSKeys._
import com.typesafe.sbt.packager.universal.UniversalKeys
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseKeys

object ApplicationBuild extends Build with UniversalKeys {

  var scalajvmProject:Option[Project] = None

  override def rootProject = { scalajvmProject }

}