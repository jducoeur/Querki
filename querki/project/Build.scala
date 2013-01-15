import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

    val appName         = "Querki"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
	  // Main Play dependencies
	  jdbc,
	  anorm,
      // Add your project dependencies here,
      //"eu.henkelmann" % "actuarius_2.9.2" % "0.2.4"
      "org.hoisted" % "actuarius_2.10" % "0.2.5-SNAPSHOT"
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
    //val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
      //scalaVersion := "2.10.0-RC1"
    )
}
