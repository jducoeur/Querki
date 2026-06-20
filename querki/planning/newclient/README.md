# New Querki Client — Planning Workspace

This directory is the planning workspace for **rewriting the Querki Scala.js client** (currently
in `querki/scalajs/`) using the Typelevel **Calico** framework
(https://github.com/armanbilge/calico).

## Goals & Constraints (from the project brief)

**Overriding principle — isolation for confidence.** The real goal is to **isolate the new client
code as much as is practical, so we can be highly confident we are not breaking the existing client
as we build the new one.** "Don't change the server / don't touch the old client" is a *means* to
that end, not the end itself. Modest, focused server-side changes — and even extremely careful,
surgical changes to the existing client — are acceptable *when they serve the isolation goal* (e.g.
moving a hardcoded contract into shared code that both clients then read, so neither can silently
drift). The bar is: change should make the two clients *more* independent and the blast radius
*smaller*, never the reverse. When in doubt, prefer additive/shared changes over edits to existing
client behavior, and call out any existing-client touch explicitly so it can be reviewed for
regression risk.

- **Parallel build, not in-place.** The existing client (`querki/scalajs/`, sbt project
  `querkiClient`) keeps shipping. The new client is a *separate* sbt sub-project, built alongside
  it. We avoid changing the existing client; where a change there genuinely helps isolation, it
  must be small, deliberate, and flagged.
- **Same server APIs by default.** The new client talks to the same Play/autowire endpoints and the
  same shared API traits (`scala/shared/.../*Functions.scala`). Server changes are not the plan, but
  are permitted when **modest and focused** — favor *additive* changes (new shared helpers/endpoints
  the old client can ignore) over changes to existing behavior the old client depends on.
- **Same UI widget libraries / facades** where reasonable (Bootstrap, jQuery facades, etc.), at
  least initially.
- **Throw away the homebrew framework**: the `Gadgets` library (`org.querki.gadgets.*`), the
  `squery` helpers, the Scala.Rx-based reactivity, and the custom client-side `Ecology`. Calico +
  Cats Effect + fs2 + Signal replaces all of that.
- **Built in Scala 3** (the server stays on 2.x for now; the greenfield client is where the 3
  migration begins). See doc 09 for the plan and the shared-code complications.
- **Tested via scenario testing** (the owner's preferred style) with nuanced 100% coverage. See
  doc 10 and the philosophy reference.
- This is a **multi-week / multi-month, incremental** effort.

## Document Index

| File | Contents |
|------|----------|
| [01-architecture.md](01-architecture.md) | How the current client works: bootstrap, Ecology, page lifecycle, the Gadget framework, the comm/API layer, the reactive (Scala.Rx) model. The "engine room." |
| [02-feature-inventory.md](02-feature-inventory.md) | Every major feature area (by package), what it does, its pages/gadgets, and the API calls it makes. The "what does it do." |
| [03-dependencies.md](03-dependencies.md) | External libraries, JS facades, `jsDependencies`, and which are dead/unmaintained. What we keep vs. drop. |
| [04-calico-mapping.md](04-calico-mapping.md) | First-pass thinking about how current concepts map onto Calico/Cats-Effect, open questions, and a possible step ordering. This is the bridge to the eventual implementation plan. |
| [05-editing-deep-dive.md](05-editing-deep-dive.md) | Full deep-dive on the `editing` package (Model Designer, Advanced Editor, instance editors, property creation, per-field auto-save). The first per-package deep dive; template for the rest. |
| [06-security-deep-dive.md](06-security-deep-dive.md) | Full deep-dive on the `security` package (per-Thing permission grid, sharing hub, members, invitations, custom roles, shareable links) and its reusable `Saveables`/`ItemList` abstractions. |
| [07-photos-deep-dive.md](07-photos-deep-dive.md) | Full deep-dive on the `photos` package (upload, thumbnails, full-size view, carousel). The one feature that bypasses autowire (raw streaming upload) and carries the heaviest dead-JS dependency cluster. |
| [08-remaining-features-deep-dive.md](08-remaining-features-deep-dive.md) | Consolidated deep-dive on the remaining smaller packages (conversations, notifications, search, console, history, publication, apps, identity+skilllevel, admin, email) + the cross-cutting `datamodel` helper. Includes a **cross-cutting patterns** section (dialogs, the non-autowire cluster, composable page-workflows, polling, shared client state) that matters for the whole rewrite. |
| [09-scala3-and-shared-code.md](09-scala3-and-shared-code.md) | The plan to build the new client in **Scala 3**, the JVM/2.13/3 interop facts, the recommended incremental path (2.13 stepping-stone → cross-build shared to 3), and the complications — chiefly **autowire** (Scala-2-only RPC) and upickle pickler derivation across the version boundary. |
| [10-testing-strategy.md](10-testing-strategy.md) | The **scenario-testing** harness design: the headline finding that a **browser-free jsdom** harness is feasible; how to "stand up the engine once" across the JVM/JS boundary (real-engine vs emulator); the `StateT[IO, UiState, A]` DSL echoing the server mid-tests; determinism/seams; and scoverage-clamped-to-100. |
| [reference-philosophy-of-testing.md](reference-philosophy-of-testing.md) | Condensed summary of the owner's 8-part "A Philosophy of Testing" Medium series — the authoritative intent behind the testing approach (scenario tests, nuanced 100%, determinism, code-for-testing). |

(Docs 01–04 cover the whole client at survey depth. Docs 05–08 are the per-package deep dives.
Docs 09–10 + the philosophy reference cover language choice and testing.
**All feature packages are now covered.** What remains undocumented is the framework/display layer
itself — the reusable `display` gadgets and the `org.querki.gadgets`/`squery` libraries — which
docs 01/03 already describe at the level needed, since they're being *replaced* wholesale rather
than ported.)

## Status / Progress Log

Keep this updated so work can resume after a quota refresh.

- **2026-06-13** — Initial inventory pass. Read the full client framework core (bootstrap,
  Ecology, Page/PageManager, Gadgets/ManagedFrag/GadgetRef, comm/autowire layer, InputGadgets,
  DataAccess) and surveyed all feature packages by census. Wrote docs 01–04. The architecture and
  dependency docs are based on close reading; the per-feature API-call lists in doc 02 are based on
  directory census + spot reading and should be verified against each `*Functions` trait when we
  actually implement that area.
- **2026-06-18** — Deep dive on the `editing` package: read all 17 files + the `EditFunctions`
  trait closely, wrote doc 05. (Planning dir is now under `querki/`.) Also clarified the project's
  overriding principle (isolation for confidence; modest server + surgical old-client changes are
  OK) across docs 04/05 and README.
- **2026-06-18** — Deep dive on the `security` package: read all 16 files + the `SecurityFunctions`
  trait, wrote doc 06. Surfaced the reusable `Saveables`/`ItemList` abstractions, the
  value/custom/inherit permission model, and the Manifest/MarcoPolo + `_getCollaborators` legacy
  dependencies.
- **2026-06-18** — Deep dive on the `photos` package: read all 6 files + the `fileupload` facade +
  the server `PhotoController`, wrote doc 07. Key findings: photos is the *only* feature bypassing
  autowire (raw streaming POST to `_photoUpload`, returns pickled `Wikitext`); client-side resize is
  disabled, so the `load-image`/`canvas-to-blob` deps are likely dead and droppable; the whole
  blueimp jQuery-File-Upload chain needs replacing with File API + fetch/XHR.
- **2026-06-18** — Deep dive on **all remaining feature packages** in one pass (conversations,
  notifications, search, console, history, publication, apps, identity+skilllevel, admin, email,
  datamodel): read the files + relevant shared traits, wrote doc 08. The standout output is the
  **cross-cutting patterns** section (§12): pervasive confirmation dialogs (need a Calico modal
  early); a *cluster of non-autowire raw-AJAX endpoints* (login/signup/invite/photos/collaborators);
  composable page-as-a-step workflows via Promise→IO; polling (not push) for live updates; and the
  cross-cutting client state (UserAccess/SkillLevel/History/Notifications) that should become shared
  Signals. **All feature packages are now deep-dived.**

### Known follow-ups / things not yet deeply read
- ~~The `editing` package~~ — **done** (doc 05).
- ~~The `security` package~~ — **done** (doc 06).
- ~~The `photos` package~~ — **done** (doc 07).
- ~~The remaining feature packages~~ — **done** (doc 08).
- The functional-test expectations (`scalajvm/test/.../functional/`) encode a lot of DOM-id
  contracts (`_pageRendered`, `_spaceLink`, etc.) the current client satisfies; the new client may
  need to honor some of these or the tests get rewritten.
- **2026-06-18** — Added the **Scala 3** plan (doc 09) and the **testing strategy** (doc 10), plus a
  reference summary of the owner's "A Philosophy of Testing" series. Key outcomes: (1) the new client
  will be Scala 3 — the dominant complication is **autowire being Scala-2-only** (recommend a bespoke
  Scala-3 RPC matching autowire's wire format to stay isolated; sloth as the later unifying option),
  preceded by a 2.13 stepping-stone for shared+server; (2) a **browser-free jsdom** scenario harness
  is feasible (Calico mounts into jsdom; munit-cats-effect drives it as `IO`), with a thin
  headless-browser tier reserved only for real-geometry cases; (3) "engine once" = boot the real
  engine in a sibling JVM and drive the JS client over loopback (recommended over a server emulator,
  but build the transport seam either way); scoverage clamped to 100 with disciplined `$COVERAGE-OFF$`.
- **Next natural step:** turn the inventory (docs 01–08) + the language/testing decisions (09/10)
  into an actual phased implementation plan — scaffolding the parallel Scala 3 sbt project, the 2.13
  shared-code stepping-stone, the transport seam + RPC choice, the shared environment/Signal model,
  the Calico modal/dialog, and the scenario harness skeleton, sequenced per doc 04 §"candidate
  ordering" / doc 08 §13.
