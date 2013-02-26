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
      //"org.hoisted" % "actuarius_2.10" % "0.2.5-SNAPSHOT"//,
      "mysql" % "mysql-connector-java" % "5.1.23"
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
    //val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
      //scalaVersion := "2.10.0-RC1"
    )
}
