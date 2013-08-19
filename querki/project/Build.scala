import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

    val appName         = "Querki"
    val appVersion      = "0.3.19"

    val appDependencies = Seq(
	  // Main Play dependencies
	  jdbc,
	  anorm,
      // Add your project dependencies here,
      //"org.hoisted" % "actuarius_2.10" % "0.2.5-SNAPSHOT"//,
      "mysql" % "mysql-connector-java" % "5.1.23",
      "org.scalatest" %% "scalatest" % "2.0.M5b" % "test",
      "javax.mail" % "javax.mail-api" % "1.5.0",
      "com.sun.mail" % "smtp" % "1.5.0",
      "com.sun.mail" % "mailapi" % "1.5.0",
      "com.github.nscala-time" %% "nscala-time" % "0.4.2"
    )
    
    val main = play.Project(appName, appVersion, appDependencies).settings(
    //val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
      //scalaVersion := "2.10.0-RC1"

      // ScalaTest, which is used by Actuarius
      //libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    )
}
