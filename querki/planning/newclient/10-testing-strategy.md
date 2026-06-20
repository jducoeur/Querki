# Testing Strategy for the New Client

How the new (Calico, Scala 3) client will be tested. This applies the project owner's own testing
philosophy (see [reference-philosophy-of-testing.md](reference-philosophy-of-testing.md)) to a
Scala.js UI, and is grounded in the **existing server-side scenario-test harness**
(`scalajvm/test/querki/test/mid/`), which is the prior art we deliberately echo.

> Terminology: the owner calls these **scenario tests** (formerly "mid tests"; sometimes "functional
> tests", a term we avoid as overloaded). The defining traits: the **real system runs in-process**,
> **external dependencies are emulated**, tests are **long, modular scenarios**, and a **single run
> exercises everything**.

## Goals (from the owner)
1. **Scenario testing as the primary style** — few tests, highly modular, one run exercises
   everything. Modular pieces should *later* be runnable individually (lower priority now).
2. **Nuanced 100% coverage** via scoverage clamped to 100, with disciplined `$COVERAGE-OFF$`.
3. **Stand up the backend Querki engine once**, run an elaborate UI scenario suite through it,
   efficient enough to run frequently.
4. **Prefer a fully-simulated DOM, no browser** if feasible; otherwise a headless browser driven by
   sbt; other ideas welcome.
5. The existing client tests are a marginally-relevant starting point only — we do better.

---

## Headline finding: the "no browser" ideal is feasible (via jsdom)

The owner suspected a browser would be unavoidable because Calico is tied to the physical DOM.
**Good news: we can almost certainly get the "completely-simulated DOM, no browser" ideal** — not by
faking the DOM ourselves, but by running on **Node.js + jsdom**:

- Scala.js has a first-class **`JSDOMNodeJSEnv`** test environment
  ([scala-js-env-jsdom-nodejs](https://github.com/scala-js/scala-js-env-jsdom-nodejs)): a headless,
  in-process DOM (`document`, `window`, elements, events) on Node, **no real browser**.
- Calico is built on **Cats Effect + fs2(-dom)**, rendering into the *standard* DOM APIs that jsdom
  implements. A component is a `Resource[IO, fs2.dom.Node]`. So Calico components mount into a jsdom
  document just fine.
- **`munit-cats-effect`** lets tests return `IO`, so a test can mount the app (acquire the
  `Resource`), drive it, and assert — all as one `IO`, no `unsafe` calls, no sleeps.

So the **primary harness is browser-free**: real client components, real Calico runtime, real-enough
DOM, driven as `IO` scenarios on Node. This is the owner's "drive the logical UI with a simulated
DOM" — achieved with a *real* (jsdom) DOM rather than a hand-built fake, which is strictly better.

### What jsdom does *not* give you (and the mitigation)
jsdom implements the DOM **API** but not a layout/rendering **engine**: no real CSS layout,
`getBoundingClientRect()` returns zeros, no painting, no true visibility-by-layout, scroll positions
are synthetic, focus/IME quirks are approximate. Anything depending on **real geometry/visual
behavior** is not faithful under jsdom. Relevant residue in this app:
- the photo full-view dialog's pixel math and the Bootstrap **carousel** (doc 07),
- drag-and-drop **reordering** geometry in the Model Designer (doc 05),
- anything asserting actual visibility via CSS rather than DOM state.

**Mitigations:** (a) the rewrite *drops most Bootstrap-JS/jQuery* (docs 03/08), removing much
browser-only behavior; (b) prefer asserting on **DOM/component state** (classes, attributes, the
presence of nodes, captured signals) rather than rendered geometry; (c) keep a **thin headless-
browser smoke tier** (below) for the genuinely visual/geometric handful.

### Optional second tier: headless browser smoke tests
Reserve a *small* browser-based suite for the things jsdom can't cover (real layout, drag-drop
geometry, visual regression). Two routes: run the same Scala.js suite under a real headless browser
via a Selenium/WebDriver `jsEnv` (e.g. headless Chrome), or a separate Playwright script. **Keep
this tier minimal** — it is the slow, flakier path the philosophy warns against; jsdom carries the
bulk. This is a later addition, not part of the initial harness.

---

## The backend: how "stand up the engine once" works across the JVM/JS boundary

The genuine wrinkle: the **engine is JVM**, the **client is Scala.js**. "In-process with the engine"
isn't literally possible across that language boundary, so "stand up the engine once" needs a
concrete shape. Three options, with a recommendation:

### Option A — Real engine in a sibling JVM, client tests over loopback HTTP *(recommended)*
Boot the **real Querki engine once** (reusing the mid-test bootstrap: real Ecology + Play, with
Cassandra/MySQL stubbed via `MidFuncDB`), then run the Node+jsdom client scenario suite against it
over `localhost` HTTP (real autowire/`_apiRequest`, real `_photoUpload`, etc.). sbt orchestrates:
start engine → wait healthy → run `clientScenario` tests → stop engine.
- **Pros:** matches the owner's explicit "real engine, once" intent; **no large server emulator to
  build or keep in sync** (avoids drift); true end-to-end confidence. Philosophically consistent
  with the series: the *engine's* external deps (DB) are the emulated part (`MidFuncDB`), exactly as
  Pt4 prescribes — the client just talks to a real, DB-stubbed engine.
- **Cons:** cross-process orchestration (the main engineering effort); real loopback HTTP (still
  fast, no internet); the JS test can't directly introspect the server `Ecology` the way JVM
  mid-tests do — so server-truth assertions stay in the JVM mid-tests, and client scenarios assert
  on UI state + observed API responses (a clean division of labor).

### Option B — Emulate the server API at the client's transport seam (pure Node)
Per Pt7, the client's one external dependency is the server API (the `*Functions` traits + the
raw-AJAX endpoints, doc 08 §12.2). Put a **transport seam** at the client's RPC boundary and, in
tests, swap a `FakeServer` emulator implementing only the API surface the client uses, with
pre-injected data, state inspection, and error injection (textbook Pt7 `FakeRqClient`).
- **Pros:** the *faithful* translation of the philosophy (emulate the system-under-test's
  externals); pure-JS, fastest inner loop, fully deterministic, no cross-process orchestration.
- **Cons:** the emulator must reproduce server semantics for everything the UI exercises — a real
  build-and-maintain cost, and a **drift risk** vs. the real engine. (Pt4's "<1% size" assumes a
  *simple* external; the Querki engine's behavior is not simple to emulate faithfully.)

### Recommendation
**Primary = Option A** (real engine, booted once) — it honors the owner's stated goal, reuses
existing infra, and sidesteps emulator drift. **But build the transport seam from day one anyway**
(it's good testable design per Pt7), so that **Option B becomes available as a fast inner-loop
variant** for individual feature scenarios without paying the engine-boot cost. In other words: the
seam is mandatory architecture; which backend a given run uses is a swappable choice. *(This A-vs-B
default is the one genuine decision to confirm before building the harness.)*

---

## The scenario DSL — echo the existing `TestOp`

The server harness models a scenario as a state monad: `TestOp[A] = StateT[IO, TestState, A]`, with
combinators (`client`, `withState`, `expectingError`, …), composed into one `FullMidTests` run that
boots the engine once and sequences sub-suites (`BasicMidTests`, `ConvMidTests`, `SecurityMidTests`,
…) via for-comprehension. **We mirror this exactly** on the client:

```
ClientScenario[A] = StateT[IO, UiState, A]   // UiState: mounted app, jsdom handles, captured API traffic, current user/space
```
Combinators (sketch), all returning `IO`/`ClientScenario`, never sleeping:
- **Navigation/driving**: `navigateTo(hash)`, `click(selector)`, `enterText(selector, s)`,
  `submit(form)` — dispatch real DOM events into the mounted Calico app.
- **Awaiting** (determinism, Pt6): `awaitRendered` / `awaitSettled` — await the component's mount
  `IO` / a readiness `Signal`, **not** a timeout. (The old client already exposes a
  `renderedContentFuture` + `#_pageRendered` signal — we keep that idea as first-class IO/Signal.)
- **Assertions**: `expectText(selector, …)`, `expectExists/Absent(selector)`,
  `expectApiCall(ThingFunctions.getThingPage, …)` (against captured transport traffic),
  `expectFlash(...)`, `expectStatusLine(...)`.
- **Server/world setup**: reuse the mid-harness vocabulary where it overlaps (create a user, create
  a Space, seed Things) — ideally by *sharing* setup code with the JVM mid-tests, since both speak
  the same `*Functions` API.
- **Error scenarios**: `expectingError[T]` like the existing harness.

A scenario is a long for-comprehension ("dozens of interactions long"). `FullClientScenario`
composes the per-feature sub-scenarios into one run that boots the engine once. Each sub-scenario
works in its **own fresh Space/session** for isolation (as the mid-tests do), which also gives us
the seam to later run a single sub-scenario alone (the owner's lower-priority want): each is just a
named value that can be run against a freshly-booted (or shared) engine.

### Modularity → individual runnability (later)
Because each feature scenario is a standalone `ClientScenario[Unit]` value, a future harness can
expose each as its own munit test that either reuses a shared engine or boots Option B's emulator.
Designing the scenarios as composable values now is what makes that cheap later — no rework.

---

## Determinism (Pt6) — concrete rules for this client
- **Clock seam**: no `new js.Date()` / `Date.now()` / moment-"now" in logic; inject a clock the test
  controls. (The old client uses moment.js; the new one needs a clock abstraction from day one.)
- **No timeouts/sleeps in tests**: await Calico mount `IO`s and `Signal` settles. This is a natural
  fit — Calico mounting *is* an `IO`, so "the page is ready" is awaitable, not pollable.
- **Kill the polling patterns** (doc 08 §12.4): notifications' post-page-load check and admin's 1s
  `setTimeout` strobe become fs2 `Stream`s with an injected scheduler the test can advance; tests
  assert on emitted values, never wait wall-clock.
- **TestHooks** (Pt7): for "nothing happened" cases (an event deliberately ignored, a save debounced)
  expose a hook the component reports to — empty in prod, captured in test — so we assert on
  *deliberate non-action* without timeouts.
- **No randomness** at test time (e.g. `PhotoList`'s random carousel id — doc 07 — must take an
  injectable id source).

## Code-for-testing (Pt7) — seams to build into the new client from day one
These shape the architecture (cross-ref doc 04's environment/Signal model and doc 08 §12):
1. **Transport seam** at the RPC boundary (enables Option A *or* B; captures traffic for assertions).
2. **Clock seam** (above).
3. **Scheduler seam** for the polling/animation timers.
4. **Readiness as IO/Signal** — every page/component exposes an awaitable "ready," replacing the
   old `#_pageRendered` DOM-signal hack.
5. **Injectable id/random source** where needed.
6. The cross-cutting client **state as Signals** (UserAccess/SkillLevel/History/Notifications, doc
   08 §12.5) is inherently testable — tests set/observe those signals directly.

## Coverage (Pt5) — scoverage clamped to 100, nuanced
- Use **sbt-scoverage** (supports Scala.js) on the new client project; set
  `coverageMinimumStmtTotal := 100` (and branch where practical), `coverageFailOnMinimum := true`.
- Coverage is measured **during the scenario run** (instrumented `fastLinkJS` under jsdom) — the
  scenarios *are* the thing that exercises the code, so 100% statement coverage forces the scenario
  suite to be genuinely comprehensive. That pressure is the point.
- **`$COVERAGE-OFF$`/`$COVERAGE-ON$`** for legitimately-unexercisable code, each with a *why*
  comment: thin Scala.js facade glue with no logic, provably-unreachable branches in
  exhaustively-typed matches, dev-only debug spew (`spewing`/`spew` from `globals`), and the
  not-yet-real headless-browser-only paths. **Never** for laziness; track real gaps with tickets.
- Edge cases a scenario genuinely can't drive → a **targeted unit test** (Pt2/Pt3 permit these for
  algorithms/data-structures/error paths: e.g. the QL/wikitext bits, the search boldface-by-position
  logic, URL hash parsing) rather than a coverage-off.
- Adoption order (Pt5): start the floor at whatever the first scenarios achieve, **ratchet to 100**
  as the suite grows; **delete dead code** rather than exempting it (the rewrite is a great moment
  for this — e.g. the `???` placeholders and vestigial code noted in docs 05–08).

## What to reuse vs. discard from existing tests
- **Discard** the old Scala.js client tests (decayed, incomplete — per the owner).
- **Reuse the *shape*** of the JVM mid-harness (`TestOp`/`StateT`, `FullMidTests` single-boot,
  `MidFuncDB` DB stubs) — and ideally **share the API-level setup vocabulary** between the JVM
  mid-tests and the new client scenarios, since both drive the same `*Functions` API. The JVM
  mid-tests remain the home of **server-truth** assertions; the client scenarios own **UI-truth**.

---

## Open decisions to confirm
1. **Backend default: Option A (real engine, recommended) vs. Option B (emulator)** for the primary
   suite. (Build the seam regardless.)
2. Whether to **share scenario/setup code** between the JVM mid-tests and the JS client scenarios
   (desirable, but cross-compiles the DSL — interacts with the Scala 3 plan, doc 09).
3. How much of the **headless-browser smoke tier** to build initially (recommend: defer; jsdom
   first).
4. Whether to honor the existing **functional-test DOM-id contracts** (`#_pageRendered`, etc.) or
   define fresh test selectors for the new client (recommend: fresh, with explicit `data-test-*`
   hooks designed for testing).
