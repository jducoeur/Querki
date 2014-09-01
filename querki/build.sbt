// This horrible magic is the build.sbt rough equivalent of Build.scala's much simpler:
//
//   override def rootProject = Some(scalajvm)
//
// In this case (if I'm understanding correctly), we're actually reloading an internal sbt that is
// set to the correct project.
//onLoad in Global := { Command.process("project scalajvm", _: State) } compose (onLoad in Global).value

val dummy = { scalajvmProject = Some(scalajvm); true }

lazy val appName = "Querki"

lazy val appVersion = "0.10.6.3"

lazy val useScalaVersion = "2.11.1"

lazy val scalajvm = (project in file("scalajvm")).enablePlugins(PlayScala).settings(scalajvmSettings: _*)

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
  libraryDependencies ++= scalajvmDependencies
)
