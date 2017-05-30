// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Needed for ConductR, according to https://github.com/typesafehub/conductr-lib
//resolvers += bintrayRepo("typesafe", "maven-releases")

// Needed for Actuarius
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

// Needed for Li Haoyi's stuff
resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.3")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.16")
addSbtPlugin("com.vmunier" % "sbt-play-scalajs" % "0.3.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-rjs" % "1.0.1")
addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.1.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-gzip" % "1.0.0")

// Adds the stats command -- see https://github.com/orrsella/sbt-stats
addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.5")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")

// For the JVM side of the shared code:
addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.0.4")

// So that the Play application can access the version and build date:
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.5.0")

// To support building with ConductR:
addSbtPlugin("com.lightbend.conductr" % "sbt-conductr" % "2.1.2")
