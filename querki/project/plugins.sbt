// Comment to get more information during initialization
logLevel := Level.Warn

// This can be used as a TEMPORARY patch if needed, to work around eviction errors in plugins.sbt while debugging:
// ThisBuild / evictionErrorLevel := Level.Warn

// The Typesafe repository
//resolvers += "Typesafe repository".at("http://repo.typesafe.com/typesafe/releases/")

// Needed for ConductR, according to https://github.com/typesafehub/conductr-lib
//resolvers += bintrayRepo("typesafe", "maven-releases")

// Needed for Actuarius
resolvers += "Sonatype OSS Snapshots".at("https://oss.sonatype.org/content/repositories/snapshots/")

// Needed for Li Haoyi's stuff
resolvers += "bintray/non".at("https://dl.bintray.com/non/maven")

dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "2.3.0"

// Use the Play sbt plugin for Play projects
addSbtPlugin(("com.typesafe.play" % "sbt-plugin" % "2.8.22").exclude("com.typesafe.sbt", "sbt-native-packager"))

addSbtPlugin("com.lightbend.sbt" % "sbt-javaagent" % "0.1.5")

addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.10.4")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.2")
addSbtPlugin("org.scala-js" % "sbt-jsdependencies" % "1.0.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

// TODO: All of these need to be *simultaneously* upgraded after we move to
// SJS 1.x! Otherwise we get cryptic errors because of conflicting versions of sbt-web. See
//   https://github.com/sbt/sbt-less/issues/133
// Do we actually care about sbt-rjs?
addSbtPlugin("com.typesafe.sbt" % "sbt-rjs" % "1.0.10")
addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.1.4")
addSbtPlugin("com.typesafe.sbt" % "sbt-gzip" % "1.0.2")
addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.2.0")

// Adds the stats command -- see https://github.com/orrsella/sbt-stats
addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.7")

// So that the Play application can access the version and build date:
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
