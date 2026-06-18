# New Querki Client — Planning Workspace

This directory is the planning workspace for **rewriting the Querki Scala.js client** (currently
in `querki/scalajs/`) using the Typelevel **Calico** framework
(https://github.com/armanbilge/calico).

## Goals & Constraints (from the project brief)

- **Parallel build, not in-place.** The existing client (`querki/scalajs/`, sbt project
  `querkiClient`) stays untouched and shipping. The new client is a *separate* sbt sub-project,
  built alongside it.
- **Same server APIs.** The new client talks to the same Play/autowire endpoints and the same
  shared API traits (`scala/shared/.../*Functions.scala`). No server changes are anticipated for
  the rewrite itself.
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

## Status / Progress Log

Keep this updated so work can resume after a quota refresh.

- **2026-06-13** — Initial inventory pass. Read the full client framework core (bootstrap,
  Ecology, Page/PageManager, Gadgets/ManagedFrag/GadgetRef, comm/autowire layer, InputGadgets,
  DataAccess) and surveyed all feature packages by census. Wrote docs 01–04. The architecture and
  dependency docs are based on close reading; the per-feature API-call lists in doc 02 are based on
  directory census + spot reading and should be verified against each `*Functions` trait when we
  actually implement that area.

### Known follow-ups / things not yet deeply read
- The `editing` package (2037 lines, 17 files) — the Model Designer / instance editor is the most
  complex single feature; only skimmed.
- The `security` package (1832 lines, 16 files) — sharing/roles/invitations UI; only skimmed.
- `photos` (file upload + image manipulation) — depends on a chain of jQuery FileUpload plugins.
- The functional-test expectations (`scalajvm/test/.../functional/`) encode a lot of DOM-id
  contracts (`_pageRendered`, `_spaceLink`, etc.) the current client satisfies; the new client may
  need to honor some of these or the tests get rewritten.
