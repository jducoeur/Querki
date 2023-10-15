import sbt.Project.projectToRef

import ByteConversions._

lazy val clients = Seq(querkiClient)

lazy val scalaV = "2.11.12"
lazy val akkaV = "2.4.18"
lazy val enumeratumV = "1.5.3"
lazy val appV = "2.10.4.1"

lazy val sharedSrcDir = "scala"

lazy val querkiServer = (project in file("scalajvm")).settings(
  scalaVersion := scalaV,
  version := appV,
  scalaJSProjects := clients,
  pipelineStages := Seq(scalaJSProd, digest, gzip),
  // Needed for the in-memory Cassandra driver, used during tests:
  resolvers += "dnvriend".at("http://dl.bintray.com/dnvriend/maven"),
  // To prevent duplicate-artifact errors in Stage:
  publishArtifact in (Compile, packageSrc) := false,
  libraryDependencies ++= sharedDependencies.value ++ Seq(
    // Main Play dependencies
    jdbc,
    // anorm,
    filters,
    "com.typesafe.play" %% "anorm" % "2.5.0",
    // Add your project dependencies here,
    "mysql" % "mysql-connector-java" % "5.1.36",
    "com.sun.mail" % "javax.mail" % "1.6.2",
    "com.sun.mail" % "smtp" % "1.5.0",
    "com.sun.mail" % "mailapi" % "1.6.2",
    "com.github.nscala-time" %% "nscala-time" % "1.6.0",
    "com.typesafe.akka" %% "akka-testkit" % akkaV,
    "com.typesafe.akka" %% "akka-contrib" % akkaV,
    "com.typesafe.akka" %% "akka-cluster-tools" % akkaV,
    "com.typesafe.akka" %% "akka-cluster-sharding" % akkaV,
    "com.typesafe.akka" %% "akka-cluster" % akkaV,
    "com.typesafe.akka" %% "akka-slf4j" % akkaV,
    "com.typesafe.akka" %% "akka-persistence" % akkaV,
    "com.typesafe.akka" %% "akka-persistence-cassandra" % "0.17",
    "com.typesafe.akka" %% "akka-persistence-query-experimental" % akkaV,
    "org.imgscalr" % "imgscalr-lib" % "4.2",
    "com.amazonaws" % "aws-java-sdk" % "1.8.4",
    "com.vmunier" %% "play-scalajs-scripts" % "0.5.0",
//    "com.lihaoyi" %% "utest" % "0.3.1",
    "org.querki" %% "requester" % "2.6",
    "com.github.mauricio" %% "mysql-async" % "0.2.16",
//      "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % "test",
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % "test",
    "com.github.romix.akka" %% "akka-kryo-serialization" % "0.4.2",
//      "com.typesafe.conductr" %% "play25-conductr-bundle-lib" % "1.4.4",
    "com.typesafe.akka" %% "akka-distributed-data-experimental" % akkaV,
//      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "org.scalatest" %% "scalatest" % "3.0.3" % "test",
    // Pretty-printer: http://www.lihaoyi.com/upickle-pprint/pprint/
    "com.lihaoyi" %% "pprint" % "0.4.1",
    "com.lihaoyi" %%% "fastparse" % "2.3.2",
    "com.lihaoyi" %% "sourcecode" % "0.2.3",
    // Powerful structural-diffing library: https://github.com/xdotai/diff
    "ai.x" %% "diff" % "1.2.0" % "test",
    // Only used for debugging at this point:
    "com.github.pathikrit" %% "better-files" % "2.17.1",
    "org.typelevel" %% "cats-core" % "2.0.0",
    "org.typelevel" %% "cats-effect" % "2.0.0",
    "co.fs2" %% "fs2-core" % "2.1.0",
    "com.github.julien-truffaut" %% "monocle-core" % "1.5.0",
    "com.github.julien-truffaut" %% "monocle-macro" % "1.5.0",
    // Updated version of the XML library:
    "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    // A simple Base64 library, for embedding stuff into HTML:
    "com.github.marklister" %% "base64" % "0.2.3",
    // We are also using BooPickle for embedding:
    "io.suzaku" %% "boopickle" % "1.2.6",
    // We use JSoup for HTML cleaning:
    "org.jsoup" % "jsoup" % "1.11.2",
    // In-memory Akka Persistence driver, used for tests. Note that this is for Akka 2.4!
    "com.github.dnvriend" %% "akka-persistence-inmemory" % "1.3.9" % "test",
    // In-memory H2 database, used for tests:
    "com.h2database" % "h2" % "1.4.192" % "test",
    // For graphql processing:
    "org.sangria-graphql" %% "sangria" % "1.4.2",
    "com.chuusai" %% "shapeless" % "2.3.3"
  ),
  // ConductR params
  BundleKeys.nrOfCpus := 1.0,
  // We have 4GB nodes. This allows for 2 simultaneous bundles per node during release, plus
  // overhead for ConductR and system. 1.5 GB seems safe in practice; we might be able to raise this to 2GB?
  BundleKeys.memory := 1536.MB,
  BundleKeys.diskSpace := 5.MB,
  BundleKeys.startCommand ++= Seq(
    "-Dhttp.address=$WEB_BIND_IP -Dhttp.port=$WEB_BIND_PORT",
    "-java-home /apps/java"
  ),
  BundleKeys.system := "querki-server",
  BundleKeys.endpoints := Map(
    "akka-remote" -> Endpoint("tcp"),
    "web" -> Endpoint("http", services = Set(URI("http://:9000")))
  ),
  // When running server tests, use this alternate config file, which uses the in-memory persistence
  // instead of on-disk:
  javaOptions in Test += "-Dconfig.file=conf/application.test.conf",
  fork in Test := true,
  // For cats:
  scalacOptions += "-Ypartial-unification",
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "querki",
  EclipseKeys.skipParents in ThisBuild := false
).enablePlugins(JavaAppPackaging, PlayScala, BuildInfoPlugin). //, ConductRPlugin).
  // TODO: this aggregate is how we pull in the client and get it to compile, but it's too broad:
  // it causes the system to run the Client during unit testing, which we don't want. We need to
  // figure out how to restructure such that the client gets *built* but not *tested*, at least
  // for now.
  aggregate(clients.map(projectToRef): _*).dependsOn(querkiSharedJvm)

lazy val querkiClient = (project in file("scalajs")).settings(
  scalaVersion := scalaV,
  version := appV,
  persistLauncher := true,
  persistLauncher in Test := false,
//  sourceMapsDirectories += file(sharedSrcDir),
  jsDependencies += RuntimeDOM,
//  postLinkJSEnv := PhantomJSEnv(autoExit = false).value,

  // Javascript libraries we require:
  skip in packageJSDependencies := false,
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
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "querki",
  libraryDependencies ++= sharedDependencies.value ++ Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.3",
    "org.scala-js" %%% "scala-parser-combinators" % "1.0.2",
    "org.scala-lang.modules" %% "scala-async" % "0.9.2",
    "org.querki" %%% "querki-jsext" % "0.8",
    "org.querki" %%% "jquery-facade" % "1.2",
    "org.querki" %%% "bootstrap-datepicker-facade" % "0.8",
    "io.github.widok" %%% "scala-js-momentjs" % "0.1.5",
    "org.querki" %%% "jstree-facade" % "0.5",
    "org.querki" %%% "squery" % "0.1",
    "org.querki" %%% "gadgets" % "0.3",
    "com.lihaoyi" %%% "fastparse" % "0.4.3"
  )
).enablePlugins(ScalaJSPlugin, ScalaJSPlay, BuildInfoPlugin).dependsOn(querkiSharedJs)

lazy val querkiShared = (crossProject.crossType(CrossType.Full) in file("scala")).settings(
  scalaVersion := scalaV,
  version := appV
).
// Needed for Twirl's Html class. Note that we must *not* use PlayScala here -- it mucks up CrossProject:
jvmConfigure(_.enablePlugins(SbtTwirl)).jvmSettings(
  libraryDependencies ++= sharedDependencies.value ++ Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  )
).jsConfigure(_.enablePlugins(ScalaJSPlay)).jsSettings(
//    sourceMapsBase := baseDirectory.value / "..",
  libraryDependencies ++= sharedDependencies.value ++ Seq(
    "org.scala-js" %%% "scala-parser-combinators" % "1.0.2"
  ),
  test := {},
  EclipseKeys.useProjectId := true
)
lazy val querkiSharedJvm = querkiShared.jvm
lazy val querkiSharedJs = querkiShared.js

lazy val sharedDependencies = Def.setting(Seq(
  "com.lihaoyi" %%% "upickle" % "0.4.3",
  "com.lihaoyi" %%% "scalarx" % "0.3.2",
  "com.lihaoyi" %%% "autowire" % "0.2.5",
  "com.lihaoyi" %%% "scalatags" % "0.6.5",
  "org.querki" %%% "shocon" % "0.4",
  "com.beachape" %%% "enumeratum" % enumeratumV,
  "com.beachape" %%% "enumeratum-upickle" % enumeratumV
))

// utst -- run the Unit Tests:
addCommandAlias("utst", """querkiServer/test-only -- -l "org.scalatest.tags.Slow"""")
// ftst -- run the Functional (browser) Tests:
addCommandAlias("ftst", """querkiServer/test-only -- -n "org.scalatest.tags.Slow"""")

onLoad in Global := (Command.process("project querkiServer", _: State)).compose((onLoad in Global).value)

// for Eclipse users
EclipseKeys.skipParents in ThisBuild := false
// Compile the project before generating Eclipse files, so that generated .scala or .class files for views and routes are present
EclipseKeys.preTasks := Seq(compile in (querkiServer, Compile))

fork in run := true

// let us know about deprecation and feature warnings
scalacOptions ++= Seq("-deprecation", "-feature")
