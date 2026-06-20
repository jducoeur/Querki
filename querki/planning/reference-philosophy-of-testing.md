# Reference: "A Philosophy of Testing" (jducoeur)

Condensed summary of Mark "Justin" Waks' 8-part Medium series, ingested 2026-06-18 for use as the
guiding testing philosophy of the new client. This is **the author's own philosophy** — treat it as
authoritative intent for how the new client's tests should be designed. Source (all Nov 2021):
[Pt1 Introduction](https://jducoeur.medium.com/a-philosophy-of-testing-1-introduction-dc5b7b43a211),
[Pt2 Idiomatic Scala](https://jducoeur.medium.com/a-philosophy-of-testing-2-idiomatic-scala-and-testing-c09a610ff21d),
[Pt3 Problem of Unit Tests](https://jducoeur.medium.com/a-philosophy-of-testing-3-the-problem-of-unit-tests-bbb1285e4078),
[Pt4 Scenario Tests](https://jducoeur.medium.com/a-philosophy-of-testing-4-scenario-tests-6cdda3e74f44),
[Pt5 The Case for 100%](https://jducoeur.medium.com/a-philosophy-of-testing-5-the-case-for-100-a0ddea055002),
[Pt6 Determinism](https://jducoeur.medium.com/a-philosophy-of-testing-6-the-quest-for-determinism-1de14586a24f),
[Pt7 Code for Testing](https://jducoeur.medium.com/a-philosophy-of-testing-7-code-for-testing-ad1092b1361),
[Pt8 Summary](https://jducoeur.medium.com/a-philosophy-of-testing-8-summary-3ca0c061ebe4).

## The throughline
- **Golden Rule:** *"Writing code is easy. Maintaining code is hard."* Optimize everything for
  maintainability; test automation is the primary tool for that.
- **Test mindfully.** A test is a hypothesis about behavior, chosen because it matters — not a
  reflex or a coverage-number chase.
- **Test code is real code.** Often larger than the app; deserves the same DRY/clarity/maintenance
  standards. Boilerplate-heavy harnesses are inexcusable.
- Testing is **part of everyday dev work** (~20–25% of coding time), not a separate QA phase.

## What idiomatic Scala means you *don't* test (Pt2)
- Prefer **strong types over tests**: make illegal states unrepresentable rather than testing for
  them. ("Always better to prevent an illegal situation with strong types than to catch it with
  unit tests.")
- **No `null`** in business logic (wrap at boundaries in `Option`) → no null-handling tests.
- **Immutability** → no need to test mutation/race/state-transition matrices.
- Net: don't write tests the compiler already makes unnecessary.

## Why not (mostly) unit tests (Pt3)
- Service code is mostly **"plumbing"** — connecting APIs/stores with mild transformations — not
  deep algorithms or data structures. Plumbing classes rarely warrant isolated tests.
- **Mocking is the problem**: mock definitions out-size the test; brittle to refactors; an
  "indirect failure" cascade (change C's semantics, and unit tests for E→F→G→C all break);
  mock-based coverage gives **false security** (passes in isolation, fails in real interaction).
- Bugs live in the **interactions between components**, which unit tests specifically don't see.
- Unit tests still earn their keep for: **complex data structures, intricate algorithms, and
  hard-to-hit error paths.**

## Scenario tests — the core method (Pt4)
- Test **the application as a whole, transparently, in-process, with heavy instrumentation.** Middle
  ground between unit and integration tests.
- **System under test runs for real** — no mocking of *internal* components. Tests call **the real
  APIs of the service.**
- **External systems are replaced with lightweight emulators** — "simple but realistic," often
  <1% the size of the real thing, implementing only what the service actually uses.
- Tests run in a normal framework (ScalaTest etc.), as **long scenarios — "dozens of API calls
  long"** — exercising realistic end-to-end workflows. Fast enough (tens–hundreds of ms) to run
  constantly, because there's no network/real-external latency.
- A scenario is a **smoke test of core flows**; keep edge cases as separate, targeted tests.

## The case for (nuanced) 100% coverage (Pt5)
- Any target below 100% is **arbitrary** — "80%" really means "I don't care which 20% goes
  untested," which violates testing mindfully. 100% is the only honest baseline.
- **"Nuanced 100%"**: every line must be **accounted for** — either tested, or **explicitly
  exempted with a documented reason.** *"100% coverage should be your baseline, not your final
  goal."*
- Legitimate exemptions (via `$COVERAGE-OFF$` / `$COVERAGE-ON$`, each with a *why* comment):
  truly-unreachable branches in exhaustively-typed code, `final val`s (inlined, never executed),
  and similar provably-dead code. Track real test debt with tickets (`TODO (FOO-123)`). **Never**
  coverage-off out of laziness.
- Practical adoption: install scoverage, set the floor at *current* coverage, ratchet upward;
  **delete untested dead code** rather than exempting it; add a test for **every post-release bug**.

## Determinism — kill flakiness (Pt6)
- Flaky tests destroy confidence in the whole suite. *"Strive to make your system as deterministic
  as possible at test time."*
- **Never `Instant.now` except for logging** — inject a controllable clock; advance time
  synthetically (also makes tests faster).
- **No randomness** at test time — substitute deterministic sequences; test edge cases explicitly
  rather than hoping random generation finds them.
- **Timeouts are a bad smell.** Don't wait-and-hope; add **test instrumentation that explicitly
  signals when an action happened** (or was deliberately skipped), and listen for the signal.
- **Concurrency:** inject test hooks into async logic — no-ops in prod, but able to block/delay
  deterministically in test to force race conditions on purpose.

## Code for testing (Pt7)
- **Testability is a core requirement, not an afterthought:** *"If your code doesn't provide a good
  means to test it, the code is incomplete."*
- **Dependency injection through abstraction**: depend on interfaces for external systems; inject
  test-controllable versions (à la ZIO's `TestClock`/`TestRandom`/`TestConsole`).
- **Abstract external services** behind your own client interface; in tests swap a `Fake*Client`
  in-memory emulator that implements only the entry points actually used, supports **pre-injected
  test data, post-run state inspection, and error injection**.
- **TestHooks pattern**: a trait the code reports events to — empty in prod, capturing in test — so
  tests can assert on *actions taken and actions deliberately avoided* (kills timeout-based asserts).
  Use **call-by-name** so prod pays no cost.

## One-line takeaways (Pt8)
Maintainability first · test mindfully · test code = real code · types over tests · no null ·
immutable · unit tests for plumbing are a net negative · scenario tests in-process with
instrumentation · 100% coverage as a nuanced baseline · determinism (no `now`, no randomness, no
timeouts) · code *for* testing with seams and hooks.
