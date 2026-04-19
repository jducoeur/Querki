# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What Querki Is

Querki is a cloud-hosted, "small data" platform — think of it as "what a wiki is to a static website, Rails is to Querki." Users build tiny, structured, data-centric apps (shopping lists, club rosters, cookbooks, inventories) without programming. The live product runs at https://querki.net. Public help lives at https://www.querki.net/help/; the "raw" rendering of a page (useful for reading outside the SPA) is available at `https://querki.net/raw/:userName/:spaceId/:thingId` — e.g. the docs home is `https://querki.net/raw/systemUser/documentation/.3y2848u`.

## Repository Layout

This is a single sbt multi-project build; **all sbt commands must be run from `./querki/`**, not the repo root. Top-level files outside `querki/` are historical (`Querki.tdl`, `Graphics/`, `test_system_template.sql`).

Inside `querki/`:

- `scalajvm/` — Play 2.8 server (project `querkiServer`). Source in `app/`, tests in `test/`, config in `conf/`.
- `scalajs/` — Scala.js browser client (project `querkiClient`). Compiles to JS that the Play server serves.
- `scala/` — sbt-crossproject `querkiShared` with `shared/`, `jvm/`, `js/` source roots. Anything needed on both sides (client ↔ server API traits, QL text parser, data models like `OID`, `Kind`, `Wikitext`) lives here.
- `project/` — sbt plugins and build meta. sbt version is pinned in `project/build.properties` (1.4.9).

Scala is pinned to `2.12.15`, Akka to `2.6.5`. Target JDK is Java 8 (Corretto in Docker). These versions are locked by transitive dependencies and comments in `build.sbt` enumerate what blocks each upgrade — treat version bumps as non-trivial.

## Files to Never Read

**Anything ignored by git is private and should not be read.** The top-level `.gitignore` is where to check. Most importantly:

- `querki/localsecrets.conf` — contains real secrets
- `querki/aws_creds`
- `querki/scalajvm/conf/application.conf` — the real config (use `application.conf.template` and `scenario.conf` / `test.conf` for reference instead)
- `*.db`, `logs/`, `target/`, `tmp/`, `testdb/`, `querki/journal/`, `querki/snapshots/`, `project/project/`, `play-fork-run.sbt`, `sbt-ui.sbt`, `local.sbt`

## Build, Run, Test

All from `./querki/`:

- `sbt compile` — builds server + client + shared together. The `querkiServer` project `.aggregate`s the Scala.js client, so a plain `compile` in `querkiServer` builds everything.
- `sbt utst` — **unit tests** (command alias). Runs `querkiServer/test-only` excluding tests tagged `org.scalatest.tags.Slow`. This is the fast, in-process test suite.
- `sbt ftst` — **functional / mid tests** (command alias). Runs only tests tagged `Slow`. Requires `QUERKI_ENV=scenario` (already set via `Test / envVars` in `build.sbt`) and uses `conf/application.test.conf`.
- `sbt "testOnly querki.foo.BarSpec"` — run a single ScalaTest spec.
- `sbt "testOnly querki.foo.BarSpec -- -z \"some text\""` — run tests whose name contains substring.
- `sbt scalafmtAll` / `sbt scalafmtCheckAll` — format or check formatting. `.scalafmt.conf` pins `version = 2.7.5`, `maxColumn = 120`, sorted imports, and `-Xfatal-warnings` is on in `build.sbt` so warnings break the build.
- `sbt docker:stage` / `sbt docker:publishLocal` — package the server into a Docker image (base `amazoncorretto:8-alpine-jre`, port 9000).
- `sbt dist` — standard Play distribution tarball.

`Global / onLoad` automatically `project querkiServer`s on sbt startup. `Test / parallelExecution := false` is set globally; don't change this lightly — tests share Ecologies and singletons.

### Runtime Environments

The server **will refuse to start without `QUERKI_ENV` set.** `QuerkiEnv` reads it and the matching sub-config block is merged on top of the base config (see `QuerkiApplicationLoader.configurationWithEnv`). Values are (at least): `dev`, `prod`, `scenario`, `test`, `testnode`. Test infrastructure sets `scenario`.

Secrets come from AWS Secrets Manager in prod (see `SecretsLoader`) and from `localsecrets.conf` locally. `secrets.conf.template` shows the expected keys (SMTP password, Play crypto secret, MySQL creds, Cassandra contact points).

## High-Level Architecture

### Three tiers: Play server, Akka cluster, Scala.js client

The Play server (`scalajvm/`) is both the HTTP front door and the host of an Akka Cluster. Controllers (`app/controllers/`) are thin — `ClientController` is the main one, translating HTTP requests into either page loads (`index`, `space`) or API calls (`apiRequest`, `rawApiRequest`). The SPA client lives in `scalajs/` and is served as compiled JS + a small bootstrapping HTML page. A separate `/raw/:user/:space/:thing` path (handled by `RawController`) renders plain HTML for crawlers and for people who find the SPA inconvenient.

The client and server talk via **autowire** RPC over a single POST endpoint (`_apiRequest`). The shared API traits (`ThingFunctions`, `EditFunctions`, `SecurityFunctions`, `ConversationFunctions`, `PublicationFunctions`, `ImportSpaceFunctions`, `AdminFunctions`, `SearchFunctions`, `HistoryFunctions`, `GraphQLFunctions`, etc.) live in `scala/shared/src/main/scala/querki/*/` — **if you change one, both sides must be touched**. The server implementations (`FooFunctionsImpl`) are in `scalajvm/app/querki/*/` and usually run inside an Akka actor (`UserSpaceSession`) that owns the user's view onto a Space.

There's also a GraphQL endpoint (`/graphql/:user/:space`) and a legacy JSON endpoint, both routed directly via controllers.

### The Ecology pattern (dependency injection)

Querki's DI system is homegrown and central. Read `scala/shared/src/main/scala/querki/ecology/Ecology.scala` and `scalajvm/app/querki/ecology/Ecology.scala` first.

- The **Ecology** is a registry of **Ecots** (formerly "Modules"). An Ecot is a stateless functional unit that can implement `EcologyInterface`s, declare Properties / Types / Collections / Things that get merged into the System Space, and optionally create Actors.
- `QuerkiRoot` (an Actor) creates the `EcologyImpl`, calls `SystemCreator.createAllEcots`, then `ecology.init(...)` which runs topologically based on `initRequires` dependencies.
- **Ecot IDs (`ecotId: Short`) are FOREVER.** They encode into every `OID` (`moid`) that the Ecot creates, and those OIDs are written to user databases. Look at the numbered comments in `SystemCreator.scala` — each Ecot has a permanent number, gaps are preserved intentionally, and removed Ecots leave their number unused. Never renumber, never reuse a number. If an Ecot is removed, comment it out and leave the slot.
- `QuerkiEcot` is the standard base class server-side; `CoreEcot` is used only by `querki.core`. The client side has its own `Ecot` hierarchy in `scalajs/src/main/scala/querki/ecology/`.
- Use `initRequires[querki.foo.Foo]` in the constructor (non-lazy val) for init-time deps; use `interface[querki.foo.Foo]` (often in a `lazy val`) for runtime access.

### Data model (in `models/` and `querki.values`)

- `Thing` is the root of everything (Kind = `Thing`, `Type`, `Property`, `Space`, or `Collection` — see `Kind.scala`).
- `OID` is the globally unique 64-bit ID. `SystemIds.sysId(local)` addresses system-global Things; `EcotIds.moid(local)` addresses per-Ecot Things (upper 16 bits = ecotId, lower 16 bits = local). There is an `oldMoid` bug to be aware of — some legacy OIDs have `ecotId << 16 + localId` with wrong precedence; they're documented in `EcotIds`.
- `SpaceState` is the immutable snapshot of everything in one Space. Most functional code receives a `SpaceState` and returns a new one.
- A `Property` is typed (`PType[T]`) and has a `Collection` (`ExactlyOne`, `Optional`, `QList`, `QSet`). Values are wrapped in `QValue`.

### Spaces, persistence, and clustering

A **Space** (a user-owned collection of Things) is the main unit of live state. Each Space is an Akka Cluster Sharded entity backed by Akka Persistence:

- `SpaceCore[RM[_]]` (in `querki/spaces/SpaceCore.scala`) is the pure/testable core logic, parameterized over a `RequestM` monad so it can run synchronously in unit tests and asynchronously in prod.
- `PersistentSpaceActor` is the real Akka Persistence actor; it journals `SpaceEvent` messages and periodically snapshots (`querki.space.snapshotInterval`, default 100).
- `SpaceRouter` / `SpaceManager` handle addressing and lifecycle.
- The event journal and snapshot store are **Cassandra** (`akka-persistence-cassandra`, using the shaded Datastax driver). MySQL is only used for user accounts, identity, and similar "system" data (`app/querki/db/`, `querki.identity.UserPersistence`).
- `SpaceChangeManager` is the extension point: other Ecots inject `SpacePlugin`s to intercept / add messages into Space processing (see `SpaceCore.pluginReceive`).

Persisted messages must be marked `UseKryo` and registered via `persistentMessages` on their Ecot (`akka-kryo-serialization` is used, initialized through `querki.persistence.KryoInit`). **Never change a persisted message's shape without an evolution** — the `querki.evolutions` package exists precisely for schema migration of journaled state.

### QL — Querki's embedded language

`querki.ql` implements QL, the user-facing expression language (seen in `[[...]]` blocks in Wikitext). `QLEcot`, `Invocation`, `QLFunction`, and `CodeType` are the core abstractions. Many Ecots contribute QL functions via `InternalMethod` / `SingleContextMethod` — grep for those to understand how new functions get registered. Wikitext parsing lives in `querki.qtext` (shared between client and server).

### Testing layers

Three distinct test styles, all under `scalajvm/test/`:

1. **Unit tests** (`querki/test/QuerkiTests.scala` base class, individual specs like `QLTests`, `TypeTests`, `PropertyTests`). Use stub Ecots (`PublicUrlStub`, `UserAccessStub`, `ControllableTimeProvider`), no real Actors. Fast. Run with `sbt utst`.
2. **Mid tests** (`querki/test/mid/`). Tagged `@Slow`, so included in `sbt ftst`. `FullMidTests` composes many sub-suites (basic, conversations, security, publication) into one engine boot to save startup time. They hit the real server via the API layer. The test DSL is in `AllFuncs.scala` / `TestOp.scala`.
3. **Functional tests** (`querki/test/functional/`). Browser-driven (Selenium-ish) tests. Heaviest to run.

`RealTestingEcot` and `TestingEcot` (`querki.test`) provide test-only hooks that production code must not call.

## Conventions and Gotchas

- **Import `querki.globals._` at the top of server files**, not a grab bag. It brings in `Ecology`, `EcologyMember`, `Future`, `Config`, `QLog`, `fut` (alias for `Future.successful`), `OID`, `SpaceState`, `PublicException`, etc. There's a parallel `querki.globals` on the Scala.js side.
- **`awaitHack` means "this is a known bug, fix it"**; `awaitIntentionally` means "verified synchronous, ok to block." Don't sprinkle new `Await.result` calls without using one of those markers.
- `-Xfatal-warnings` + `-Ywarn-unused:imports` are on. Unused imports will break the build. Twirl's spurious unused-import warnings are specifically silenced in `build.sbt`.
- The server-side `Ecology` type is `EcologyBase[SpaceState, EcotImpl]`; the client's is different (lives in `querki.ecology` within `scalajs/`). Shared code uses the abstract `EcologyBase[ST, ET]` form.
- Routes are in `scalajvm/conf/routes`. The main ones: `/u/:user/:space/` → SPA; `/raw/...` → server-rendered HTML; `/_apiRequest` and `/u/:user/:space/_apiRequest` → autowire RPC; `/graphql/:user/:space` → GraphQL.
- `build.sbt` contains a number of `TODO: upgrade when ...` notes that document the dependency chain blocking Play, Akka, Scala, and sbt upgrades. When touching deps, respect those chains.
- The build has `pipelineStages := Seq(digest, gzip)` and uses `sbt-web-scalajs`; the Scala.js stage is forced to `FullOptStage` in the client project because sbt-web-scalajs otherwise only emits fastOpt.

## Where to Look First

- **New API function (client calls server)**: define the trait in `scala/shared/src/main/scala/querki/<area>/<Area>Functions.scala`, implement it in `scalajvm/app/querki/<area>/<Area>FunctionsImpl.scala`, call it from the client via `autowire` in `scalajs/src/main/scala/querki/`.
- **New QL function**: find an existing Ecot in the relevant domain (e.g. `querki.logic.LogicModule`, `querki.collections.CollectionsModule`) and add a `new InternalMethod(...)` under its `lazy val props`.
- **New Property or Type available to users**: same pattern — add to an Ecot's `props` / `types` / `things` seqs; `CoreEcot.addSystemObjects` merges them into the System Space automatically.
- **Changing persisted events**: add a new message class with a fresh `(ecotId, localId)` in `persistentMessages`, write an evolution, and never delete the old one.
