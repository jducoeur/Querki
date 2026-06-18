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

(Docs 01–04 cover the whole client at survey depth. Docs 05+ are the per-package deep dives that
each feature milestone wants before implementation — `conversations`, `history`, `apps`, etc. will
get their own as we reach them.)

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

### Known follow-ups / things not yet deeply read
- ~~The `editing` package~~ — **done** (doc 05).
- ~~The `security` package~~ — **done** (doc 06).
- ~~The `photos` package~~ — **done** (doc 07).
- The functional-test expectations (`scalajvm/test/.../functional/`) encode a lot of DOM-id
  contracts (`_pageRendered`, `_spaceLink`, etc.) the current client satisfies; the new client may
  need to honor some of these or the tests get rewritten.
