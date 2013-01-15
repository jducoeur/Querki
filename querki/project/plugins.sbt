// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Needed for Actuarius
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.1-RC1")
//addSbtPlugin("play" % "sbt-plugin" % "2.1-RC2")
//addSbtPlugin("play" % "sbt-plugin" % "2.1.0")