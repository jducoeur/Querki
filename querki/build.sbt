import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseKeys

// HACK: scalajvmProject is declared in Build.scala. We need to set this to set the rootProject:
val dummy = { scalajvmProject = Some(scalajvm); true }

val scalajsOutputDir = Def.settingKey[File]("directory for javascript files output by scalajs")

lazy val sharedSrcDir = "scala"
  
lazy val appName = "Querki"

lazy val appVersion = "0.10.6.3"

lazy val useScalaVersion = "2.11.1"

lazy val scalajvm = (project in file("scalajvm")).enablePlugins(PlayScala).settings(scalajvmSettings:_*)

lazy val scalaShared = (project in file(sharedSrcDir)).settings(scalaSharedSettings:_*)

lazy val scalajs = (project in file("scalajs")).settings(scalajsSettings:_*)

/////////////////////////////

lazy val scalajvmDependencies = Seq(
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
      "com.amazonaws" % "aws-java-sdk" % "1.8.4"
)

lazy val scalajvmSettings = Seq(
  name := appName,
  version := appVersion,
  scalaVersion := useScalaVersion,
  libraryDependencies ++= scalajvmDependencies,
//  scalajsOutputDir := (crossTarget in Compile).value / "classes" / "public" / "javascripts",
  compile in Compile <<= (compile in Compile) dependsOn (fastOptJS in (scalajs, Compile)),
  dist <<= dist dependsOn (fullOptJS in (scalajs, Compile)),
  commands += preStartCommand,
  EclipseKeys.skipParents in ThisBuild := false
//) ++ (
//  // ask scalajs project to put its outputs in scalajsOutputDir
//  Seq(packageExternalDepsJS, packageInternalDepsJS, packageExportedProductsJS, packageLauncher, fastOptJS, fullOptJS) map { packageJSKey =>
//    crossTarget in (scalajs, Compile, packageJSKey) := scalajsOutputDir.value
//  }
) ++ sharedDirectorySettings

lazy val sharedDirectorySettings = Seq(
  unmanagedSourceDirectories in Compile += new File((file(".") / sharedSrcDir / "src" / "main" / "scala").getCanonicalPath),
  unmanagedSourceDirectories in Test += new File((file(".") / sharedSrcDir / "src" / "test" / "scala").getCanonicalPath)
)

lazy val scalaSharedSettings =
  Seq(
    name := appName + "-shared",
    EclipseKeys.skipProject := true
  )
  
lazy val scalajsDependencies = Seq(
	  "org.scala-lang.modules.scalajs" %%%! "scalajs-dom" % "0.6",
      "org.scala-lang.modules.scalajs" %%%! "scalajs-jquery" % "0.6",
      "org.scalatest" %% "scalatest" % "2.2.0" % "test",
      "com.lihaoyi" %%%! "utest" % "0.2.3" % "test"
    )

lazy val scalajsSettings =
  scalaJSSettings ++ Seq(
    name := appName + "-client",
    version := appVersion,
    scalaVersion := useScalaVersion,
    persistLauncher := true,
    persistLauncher in Test := false,
    libraryDependencies ++= scalajsDependencies
  ) ++ sharedDirectorySettings


// The new 'start' command optimises the JS before calling the Play 'start' renamed 'play-start'
val preStartCommand = Command.args("start", "<port>") { (state: State, args: Seq[String]) =>
  Project.runTask(fullOptJS in (scalajs, Compile), state)
  state.copy(remainingCommands = ("play-start " + args.mkString(" ")) +: state.remainingCommands)
}
