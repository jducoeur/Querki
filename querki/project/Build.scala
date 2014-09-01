import sbt._
import Keys._
import com.typesafe.sbt.packager.universal.UniversalKeys

object ApplicationBuild extends Build with UniversalKeys {

  var scalajvmProject:Option[Project] = None
  
  override def rootProject = { scalajvmProject }

}