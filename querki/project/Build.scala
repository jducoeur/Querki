import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "Querki"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
      // Merde: there apparently isn't a Play build against RC1 yet!
      //scalaVersion := "2.10.0-RC1"
    )

}
