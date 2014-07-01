// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Needed for Actuarius
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.1")
//addSbtPlugin("play" % "sbt-plugin" % "2.1.0")
//addSbtPlugin("play" % "sbt-plugin" % "2.1-RC2")
//addSbtPlugin("play" % "sbt-plugin" % "2.1.0")


// Include the Scala.js machinery:
//resolvers += Resolver.url("scala-js-releases",
//    url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
//    Resolver.ivyStylePatterns)

//addSbtPlugin("org.scala-lang.modules.scalajs" % "scalajs-sbt-plugin" % "0.2.1")
//libraryDependencies += sbtPluginExtra(
//    m = "org.scala-lang.modules.scalajs" % "scalajs-sbt-plugin" % "0.2.1", // Plugin module name and version
//    sbtV = "0.13",    // SBT version
//    scalaV = "2.10"    // Scala version compiled the plugin
//)