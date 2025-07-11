import sbt.Project.projectToRef

import com.typesafe.sbt.packager.docker._
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val clients = Seq(querkiClient)

lazy val scalaV = "2.12.15"
lazy val akkaV = "2.5.26"
lazy val appV = "3.0.0.6-2"

lazy val sharedSrcDir = "scala"

val querkiScalacOptions = Seq(
  "-deprecation",
  "-feature",
  // Higher-kinded types stop giving a warning around Scala 2.13.1; remove this flag then:
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfatal-warnings",
  "-Ypartial-unification",
  // TODO: turn this back on and keep fixing excess imports:
//  "-Ywarn-unused:imports",
)

ThisBuild / Test / parallelExecution := false

lazy val querkiServer = (project in file("scalajvm")).settings(
  scalaVersion := scalaV,
  version := appV,
  scalaJSProjects := clients,
  pipelineStages := Seq(digest, gzip),
  Assets / pipelineStages := Seq(scalaJSPipeline),
  // Needed for the in-memory Cassandra driver, used during tests:
//  resolvers += "dnvriend".at("http://dl.bintray.com/dnvriend/maven"),
  // To prevent duplicate-artifact errors in Stage:
  Compile / packageSrc / publishArtifact := false,
  libraryDependencies ++= sharedDependencies.value ++ Seq(
    // Main Play dependencies
    jdbc,
    // anorm,
    filters,
    guice,
    "org.playframework.anorm" %% "anorm" % "2.7.0",
    // TODO: this was later renamed mysql-connector-j and has been maintained as such. Upgrade in due course:
    "mysql" % "mysql-connector-java" % "5.1.36",
    // TODO: this has apparently become jakarta.mail-api and moved on to a new major version. Also needs upgrading:
    "com.sun.mail" % "javax.mail" % "1.6.2",
    // TODO: currently up to 2.0.1 -- do the major bump eventually:
    "com.sun.mail" % "smtp" % "1.5.0",
    "com.sun.mail" % "mailapi" % "1.6.2",
    "com.github.nscala-time" %% "nscala-time" % "3.0.0",
    "com.typesafe.akka" %% "akka-testkit" % akkaV,
    "com.typesafe.akka" %% "akka-contrib" % akkaV,
    "com.typesafe.akka" %% "akka-cluster-tools" % akkaV,
    "com.typesafe.akka" %% "akka-cluster-sharding" % akkaV,
    "com.typesafe.akka" %% "akka-cluster" % akkaV,
    "com.typesafe.akka" %% "akka-slf4j" % akkaV,
    "com.typesafe.akka" %% "akka-persistence" % akkaV,
    "com.typesafe.akka" %% "akka-persistence-cassandra" % "0.59",
    "com.typesafe.akka" %% "akka-persistence-query" % akkaV,
    "com.typesafe.akka" %% "akka-distributed-data" % akkaV,
    "org.imgscalr" % "imgscalr-lib" % "4.2",
    // TODO: need to migrate this to 2.x before EOY 2025!
    "com.amazonaws" % "aws-java-sdk" % "1.12.99",
    // TODO: pull back out into a library after upgrades:
//    "org.querki" %% "requester" % "2.6",
    "io.altoo" %% "akka-kryo-serialization" % "1.1.0",
    // TODO: upgrade after we're on sbt 1.x:
    "com.lihaoyi" %% "pprint" % "0.9.0",
    "com.lihaoyi" %% "sourcecode" % "0.4.2",
    // Only used for debugging at this point; uncomment when needed:
    // "com.github.pathikrit" %% "better-files" % "3.9.2",
    "org.typelevel" %% "cats-core" % "2.13.0",
    "org.typelevel" %% "cats-effect" % "2.5.5",
    "co.fs2" %% "fs2-core" % "2.5.12",
    "com.github.julien-truffaut" %% "monocle-core" % "2.1.0",
    "com.github.julien-truffaut" %% "monocle-macro" % "2.1.0",
    "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
    // A simple Base64 library, for embedding stuff into HTML:
    // TODO: can/should we replace this with upickle? I don't think base64 is gaining us anything interesting:
    "com.github.marklister" %% "base64" % "0.3.0",
    // We are also using BooPickle for embedding:
    // TODO: at this point, we probably should replace this with upickle, using one of the compact encodings:
    "io.suzaku" %% "boopickle" % "1.5.0",
    // We use JSoup for HTML cleaning:
    "org.jsoup" % "jsoup" % "1.11.2",
    // For graphql processing:
    "org.sangria-graphql" %% "sangria" % "3.5.3",
    "com.chuusai" %% "shapeless" % "2.3.13",
    //
    // TEST-ONLY DEPENDENCIES:
    // ScalaTest can't upgrade until scalatestplus-play does, and *that* can't upgrade until we hit Play 2.8. So
    // this is a bit stuck for now:
    "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.3" % "test",
    // Powerful structural-diffing library: https://github.com/xdotai/diff
    // This will need replacing with a different library after Scala 2.12!
//    "ai.x" %% "diff" % "2.0.1" % "test",
    // In-memory H2 database, for simulating MySQL:
    "com.h2database" % "h2" % "1.4.192" % "test",
    // In-memory Akka Persistence driver, used for tests. Probably need to switch to a fork for Akka 2.6!
    // Eg, https://github.com/firstbirdtech/akka-persistence-inmemory and thence
    // https://github.com/alstanchev/pekko-persistence-inmemory
    "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.5.15.2" % "test"
  ),
  // Docker configuration
  // For now, we're using the full OpenJDK image. That's a bit fatty -- is there a leaner one for us to start with?
  dockerBaseImage := "openjdk:8-jre-alpine",
  dockerExposedPorts := Seq(9000),
  // We need bash to be present in the Docker image; otherwise, it won't boot. So this installs that as part of
  // setup:
  dockerCommands := dockerCommands.value.flatMap {
    case cmd @ Cmd("FROM", _) => List(cmd, Cmd("RUN", "apk update && apk add bash"))
    case other                => List(other)
  },
  // When running server tests, use this alternate config file, which uses the in-memory persistence
  // instead of on-disk:
  Test / javaOptions += "-Dconfig.file=conf/application.test.conf",
  Test / fork := true,
  // So that the FullMidTests can run:
  Test / envVars := Map("QUERKI_ENV" -> "scenario"),
  // For cats:
  scalacOptions ++= querkiScalacOptions,
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "querki"
)
// NOTE: we need to turn on akka-http and turn off Netty, because the version of Netty built into
// Play 2.5 conflicts with the version in the AWS SDK:
  .enablePlugins(JavaAppPackaging, PlayScala, BuildInfoPlugin, PlayAkkaHttpServer, DockerPlugin, SbtWeb)
  .disablePlugins(PlayNettyServer)

  // TODO: this aggregate is how we pull in the client and get it to compile, but it's too broad:
  // it causes the system to run the Client during unit testing, which we don't want. We need to
  // figure out how to restructure such that the client gets *built* but not *tested*, at least
  // for now.
  .aggregate(clients.map(projectToRef): _*).dependsOn(querkiSharedJvm)

def toPathMapping(f: File): (File, String) = f -> f.getName

lazy val querkiClient = (project in file("scalajs")).settings(
  scalaVersion := scalaV,
  version := appV,
  scalacOptions ++= querkiScalacOptions,
  scalaJSUseMainModuleInitializer := true,
//  sourceMapsDirectories += file(sharedSrcDir),

  // Javascript libraries we require:
  packageJSDependencies / skip := false,
  // Turn off client-side unit testing for now:
  test := {},
  jsDependencies += ("org.webjars" % "jquery" % "2.2.1" / "jquery.js").minified("jquery.min.js"),
  jsDependencies += (ProvidedJS / "jquery-ui-1.10.0.custom.js").minified("jquery-ui-1.10.0.custom.min.js").dependsOn(
    "jquery.js"
  ),
  jsDependencies += (ProvidedJS / "jquery.manifest.js").minified("jquery.manifest.min.js").dependsOn("jquery.js"),
  jsDependencies += (ProvidedJS / "jquery.ui.touch-punch.js").minified("jquery.ui.touch-punch.min.js").dependsOn(
    "jquery-ui-1.10.0.custom.js"
  ),
  jsDependencies += ("org.webjars" % "bootstrap" % "3.3.6" / "bootstrap.js").minified("bootstrap.min.js").dependsOn(
    "jquery.js"
  ),
  jsDependencies += (ProvidedJS / "jquery.autosize.min.js").dependsOn("jquery.js"),
  jsDependencies += (ProvidedJS / "jquery.raty.js").minified("jquery.raty.min.js").dependsOn("jquery.js"),
  jsDependencies += (ProvidedJS / "jquery.histogram.js").dependsOn("jquery.js"),
  jsDependencies += ("org.webjars" % "bootstrap-datepicker" % "1.6.1" / "bootstrap-datepicker.js").minified(
    "bootstrap-datepicker.min.js"
  ).dependsOn("bootstrap.js"),
  jsDependencies += (ProvidedJS / "load-image.min.js").dependsOn("jquery-ui-1.10.0.custom.js"),
  jsDependencies += (ProvidedJS / "canvas-to-blob.min.js").dependsOn("load-image.min.js"),
  jsDependencies += (ProvidedJS / "jquery.iframe-transport.js").minified("jquery.iframe-transport.min.js").dependsOn(
    "load-image.min.js"
  ),
  jsDependencies += (ProvidedJS / "jquery.fileupload.js").minified("jquery.fileupload.min.js").dependsOn(
    "jquery.iframe-transport.js"
  ),
  jsDependencies += (ProvidedJS / "jquery.fileupload-process.js").minified(
    "jquery.fileupload-process.min.js"
  ).dependsOn("jquery.fileupload.js"),
  jsDependencies += (ProvidedJS / "jquery.fileupload-image.js").minified("jquery.fileupload-image.min.js").dependsOn(
    "jquery.fileupload.js"
  ),
  // Formerly in the jsTree Facade library. We *might* remove this line when that gets pulled back out to be a library,
  // but it may make more sense to change the library to not include this itself (so as to avoid eviction problems):
  jsDependencies += ("org.webjars" % "jstree" % "3.2.1" / "jstree.js").minified("jstree.min.js"),
  // This currently has dependency issues, so we're instead making it ProvidedJS for now. At some point, see if we
  // can iron those out and do this properly:
  // jsDependencies += ("org.webjars.npm" % "moment" % "2.22.2" / "moment.js").minified("moment.min.js"),
  // jsDependencies += ("org.webjars.npm" % "moment-timezone" % "0.5.25" / "moment-timezone.js"),
  jsDependencies += (ProvidedJS / "moment.js").minified("moment.min.js"),
  jsDependencies += (ProvidedJS / "moment-timezone.js").dependsOn("moment.js"),
  Compile / fastLinkJS / jsMappings += toPathMapping((Compile / packageJSDependencies).value),
  Compile / fullLinkJS / jsMappings += toPathMapping((Compile / packageMinifiedJSDependencies).value),
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "querki",
  // Without this, sbt-web-scalajs only outputs the fastOpt files, but when we dockerize we expect the fullOpt ones:
  scalaJSStage := FullOptStage,
  libraryDependencies ++= sharedDependencies.value ++ Seq(
    // Necessary in order to get performant and reliable Futures in SJS. See
    //   https://github.com/scala-js/scala-js-macrotask-executor
    "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1",
    "com.lihaoyi" %%% "scalarx" % "0.4.3",
    // Note that upgrading this requires matching versions of moment.js and moment-timezone above. See matrix at
    //   https://github.com/vpavkin/scala-js-momentjs
    "ru.pavkin" %%% "scala-js-momentjs" % "0.10.3",
    "org.querki" %%% "querki-jsext" % "0.12",
    "org.querki" %%% "jquery-facade" % "2.1",
    // TODO: after evolving everything, pull these back out to their libraries:
    // "org.querki" %%% "bootstrap-datepicker-facade" % "0.9",
    // "org.querki" %%% "jstree-facade" % "0.5",
    // "org.querki" %%% "squery" % "0.1"
    // "org.querki" %%% "gadgets" % "0.3"
  )
).enablePlugins(ScalaJSPlugin, ScalaJSWeb, JSDependenciesPlugin, BuildInfoPlugin).dependsOn(querkiSharedJs)

// See https://github.com/portable-scala/sbt-crossproject/tree/v0.5.0?tab=readme-ov-file#migration-from-scalajs-default-crossproject
lazy val querkiShared =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("scala"))
    .settings(
      scalaVersion := scalaV,
      version := appV
    ).
// Needed for Twirl's Html class. Note that we must *not* use PlayScala here -- it mucks up CrossProject:
    jvmConfigure(_.enablePlugins(SbtTwirl)).jvmSettings(
      scalacOptions ++= querkiScalacOptions,
      libraryDependencies ++= sharedDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
      )
    ).jsConfigure(_.enablePlugins(ScalaJSWeb, JSDependenciesPlugin)).jsSettings(
//    sourceMapsBase := baseDirectory.value / "..",
      scalacOptions ++= querkiScalacOptions,
      libraryDependencies ++= sharedDependencies.value ++ Seq(
        "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"
      ),
      test := {}
    )
lazy val querkiSharedJvm = querkiShared.jvm
lazy val querkiSharedJs = querkiShared.js

lazy val sharedDependencies = Def.setting(Seq(
  "com.lihaoyi" %%% "upickle" % "4.2.1",
  "com.lihaoyi" %%% "autowire" % "0.3.3",
  "com.lihaoyi" %%% "scalatags" % "0.13.1",
  "com.lihaoyi" %%% "fastparse" % "3.1.1"
  // TODO: pull this back out into a library again after we're done with upgrades:
//  "org.querki" %%% "shocon" % "0.4",
))

// utst -- run the Unit Tests:
addCommandAlias("utst", """querkiServer/test-only -- -l "org.scalatest.tags.Slow"""")
// ftst -- run the Functional (browser) Tests:
addCommandAlias("ftst", """querkiServer/test-only -- -n "org.scalatest.tags.Slow"""")

Global / onLoad := (Global / onLoad).value.andThen("project querkiServer" :: _)

// Do we care about this? We essentially never use "run" any more; we run inside Docker instead:
run / fork := true
