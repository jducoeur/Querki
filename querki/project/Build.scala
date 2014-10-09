import sbt._
import Keys._
import play.Play._
import play.Play.autoImport._
import PlayKeys._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
import com.typesafe.sbt.packager.universal.UniversalKeys
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseKeys

object ApplicationBuild extends Build with UniversalKeys {

  val scalajsOutputDir = Def.settingKey[File]("directory for javascript files output by scalajs")

  override def rootProject = Some(scalajvm)

  val sharedSrcDir = "scala"
  
  lazy val scalajvm = Project(
    id = "scalajvm",
    base = file("scalajvm")
  ) enablePlugins (play.PlayScala) settings (scalajvmSettings: _*) aggregate (scalajs)

  lazy val scalajs = Project(
    id   = "scalajs",
    base = file("scalajs")
  ) settings (scalajsSettings: _*)

  lazy val sharedScala = Project(
    id = "sharedScala",
    base = file(sharedSrcDir)
  ) settings (sharedScalaSettings: _*)

  lazy val scalajvmSettings =
    Seq(
      name := "Querki",
      version := Versions.app,
      scalaVersion := Versions.scala,
	  
// Uncomment this line to unfold macros. WARNING: this produces copious output!
//	  scalacOptions += "-Ymacro-debug-lite",
	  
      scalajsOutputDir := (crossTarget in Compile).value / "classes" / "public" / "javascripts",
      compile in Compile <<= (compile in Compile) dependsOn (fastOptJS in (scalajs, Compile)),
      dist <<= dist dependsOn (fullOptJS in (scalajs, Compile)),
      stage <<= stage dependsOn (fullOptJS in (scalajs, Compile)),
      libraryDependencies ++= Dependencies.scalajvm,
      commands += preStartCommand,
      EclipseKeys.skipParents in ThisBuild := false
    ) ++ (
      // ask scalajs project to put its outputs in scalajsOutputDir
      Seq(packageExternalDepsJS, packageInternalDepsJS, packageExportedProductsJS, packageLauncher, fastOptJS, fullOptJS) map { packageJSKey =>
        crossTarget in (scalajs, Compile, packageJSKey) := scalajsOutputDir.value
      }
    ) ++ sharedDirectorySettings

  lazy val scalajsSettings =
    scalaJSSettings ++ Seq(
      name := "Querki-client",
      version := Versions.app,
      scalaVersion := Versions.scala,
      persistLauncher := true,
      persistLauncher in Test := false,
	  relativeSourceMaps := true,
	  // These are to give Rhino a pseudo-DOM for testing:
	  jsDependencies += scala.scalajs.sbtplugin.RuntimeDOM,
      libraryDependencies ++= Dependencies.scalajs
    ) ++ sharedDirectorySettings ++ utest.jsrunner.Plugin.utestJsSettings

  lazy val sharedScalaSettings =
    Seq(
      name := "Querki-shared",
      EclipseKeys.skipProject := true,
      libraryDependencies ++= Dependencies.shared
    )

  lazy val sharedDirectorySettings = Seq(
    unmanagedSourceDirectories in Compile += new File((file(".") / sharedSrcDir / "src" / "main" / "scala").getCanonicalPath),
    unmanagedSourceDirectories in Test += new File((file(".") / sharedSrcDir / "src" / "test" / "scala").getCanonicalPath),
    unmanagedResourceDirectories in Compile += file(".") / sharedSrcDir / "src" / "main" / "resources",
    unmanagedResourceDirectories in Test += file(".") / sharedSrcDir / "src" / "test" / "resources"
  )

  // Use reflection to rename the 'start' command to 'play-start'
  Option(play.Play.playStartCommand.getClass.getDeclaredField("name")) map { field =>
    field.setAccessible(true)
    field.set(playStartCommand, "play-start")
  }

  // The new 'start' command optimises the JS before calling the Play 'start' renamed 'play-start'
  val preStartCommand = Command.args("start", "<port>") { (state: State, args: Seq[String]) =>
    Project.runTask(fullOptJS in (scalajs, Compile), state)
    state.copy(remainingCommands = ("play-start " + args.mkString(" ")) +: state.remainingCommands)
  }
}

object Dependencies {
  val shared = Seq()

  val scalajvm = Seq(
	  // Main Play dependencies
	  jdbc,
	  anorm,
      // Add your project dependencies here,
      "mysql" % "mysql-connector-java" % "5.1.23",
      "org.scalatest" %% "scalatest" % "2.2.0",
      "javax.mail" % "javax.mail-api" % "1.5.0",
      "com.sun.mail" % "smtp" % "1.5.0",
      "com.sun.mail" % "mailapi" % "1.5.0",
      "com.github.nscala-time" %% "nscala-time" % "1.2.0",
      "com.typesafe.akka" %% "akka-testkit" % "2.3.4",
      "org.imgscalr" % "imgscalr-lib" % "4.2",
      "com.amazonaws" % "aws-java-sdk" % "1.8.4",
	  "com.lihaoyi" %% "upickle" % "0.2.5",
	  "com.lihaoyi" %% "autowire" % "0.2.3"
  ) ++ shared

  val scalajs = Seq(
	  "org.scala-lang.modules.scalajs" %%%! "scalajs-dom" % "0.6",
      "org.scala-lang.modules.scalajs" %%%! "scalajs-jquery" % "0.6",
      "org.scalatest" %% "scalatest" % "2.2.0" % "test",
      "com.lihaoyi" %%%! "utest" % "0.2.4" % "test",
	  "org.scalajs" %%%! "scala-parser-combinators" % "1.0.2",
	  "com.lihaoyi" %%%! "upickle" % "0.2.5",
	  "com.scalatags" %%%! "scalatags" % "0.4.0",
	  "com.lihaoyi" %%%! "autowire" % "0.2.3",
	  "org.scala-lang.modules" %% "scala-async" % "0.9.2"
  ) ++ shared
}

object Versions {
  val app = "0.10.7"
  val scala = "2.11.1"
  val scalajsDom = "0.6"
}
