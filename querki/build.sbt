name := "Querki"

version := "0.9.2"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

val appDependencies = Seq(
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
      "com.typesafe.akka" %% "akka-testkit" % "2.3.4"
)

libraryDependencies ++= appDependencies
