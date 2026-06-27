# Scala 3 for the New Client (and the Shared-Code Problem)

The owner wants to build the new client in **Scala 3**, using the greenfield rewrite to *begin* the
migration. Migrating the **server** to 3 is explicitly out of scope for now (over-ambitious), but
the new client is the lowest-risk place to start, and it forces useful groundwork on the shared
code. This doc lays out the interop facts, the recommended path, and the complications — the biggest
of which live in `querkiShared`.

This interacts with the testing plan (doc 10): sharing scenario/DSL code between the JVM mid-tests
and the JS client scenarios is affected by the same cross-version constraints.

## Why Scala 3 here, now
- The client is **greenfield** (parallel project, doc 04 isolation strategy) — no migration risk to
  existing code if we keep the boundary clean.
- Calico + Cats Effect + fs2 are **Scala-3-native** (3 is their primary target), so the new client's
  *own* stack has no 3 obstacle.
- It starts the org-wide 3 migration at its cheapest point and surfaces the shared-code issues early,
  while they're small.

## The hard interop facts (these drive everything)
1. **Scala 3 uses the Scala 2.13 standard library.** Scala 3 ↔ Scala **2.13** library interop is
   supported in both directions (with caveats). Scala 3 **cannot** consume Scala **2.12** artifacts.
   → Anything the Scala 3 client depends on must be available as a **2.13 or 3** artifact.
2. **Macros do not cross the 2.13/3 boundary.** A macro compiled under Scala 2 cannot be expanded in
   a Scala 3 compilation (and vice-versa). This is the crux of most complications below — it hits
   **autowire** and **upickle derivation**, both of which the shared code and client rely on.
3. `querkiShared` is currently **Scala 2.12**, cross-built JVM+JS; the server depends on
   `querkiSharedJvm` at 2.12. So today the shared artifact is unusable from a Scala 3 client.

## Recommended path (incremental, isolation-respecting)
Aligned with the owner's stated preference for incremental upgrades and additive/isolating changes
(docs 04, memory):

1. **Bump `querkiShared` + server from 2.12 → 2.13 first.** This is a *much* smaller, well-trodden
   upgrade than →3, and it's the enabling step for everything else: 2.13 has good `-Xsource:3`, so
   shared source can be made to compile cleanly under both 2.13 and 3. *(This is a modest server
   change — exactly the kind doc 04 sanctions when it serves the isolation goal: it lets the shared
   contract feed both clients without forking it.)* It can be done and shipped on its own, well
   before the new client exists.
2. **Cross-build `querkiShared` to `Seq("2.13.x", "3.x")`.** The JVM side keeps feeding the (2.13)
   server; the JS side now also publishes a **Scala 3** artifact for the new client. We rely on
   *cross-compilation*, **not** 2.13→3 binary interop, specifically because of fact #2 (picklers must
   be derived in the 3 artifact — see below).
3. **New client = Scala 3**, depends on `querkiSharedJs` (3 variant) + the Calico/CE/fs2 stack.
4. **Resolve the RPC layer** (the #1 blocker — next section).
5. **Verify shared deps' Scala 3 readiness** (mostly fine — see "dependency readiness").

> Why not rely on Scala 3 consuming the **2.13** shared artifact directly (skipping the 3
> cross-build)? Because the shared types carry **upickle picklers** (`implicit val rw = macroRW`).
> A pickler *instance* derived under upickle_2.13 belongs to the upickle_2.13 runtime; the 3 client
> must use upickle_3 and cannot reuse those instances. So the shared module must be **compiled under
> Scala 3 / upickle_3** to give the client usable picklers. Hence cross-build, not interop.

## The #1 complication: autowire (RPC) on Scala 3
**autowire (lihaoyi) is Scala 2 only** — its client `.call()` and server router are Scala-2 macros.
The new client's `.call()` site would run under Scala 3, where that macro cannot be expanded. This
is the single biggest issue, and it's *not* in shared (the `*Functions` traits are plain Scala) —
it's in the **client's and server's** autowire usage.

Options:
- **(b) Bespoke Scala 3 RPC matching autowire's wire format *(recommended short-term)*.** autowire's
  protocol is simple: POST to one endpoint with the method "path" segments + upickle-pickled args;
  the server's autowire router dispatches. The new client can reproduce *just the client half* in
  Scala 3 (an `inline`/macro or even hand-written stubs over the finite `*Functions` API) that emits
  the same request shape. **Server and old client are untouched** (they keep autowire) — maximal
  isolation. The cost is reproducing the encoding faithfully (modest; the hard part autowire's macro
  did is turning `Client[T].m(args)` into `(path, pickledArgs)`, straightforward in Scala 3
  metaprogramming).
- **(a) Migrate both ends to [sloth](https://github.com/cornerman/sloth) *(unifying, later)*.**
  Sloth is a type-safe RPC lib for **Scala 2 and 3**, explicitly inspired by autowire, with *no
  call-site macro* (one macro to build the API-trait instance). It would give one RPC mechanism
  across server (2.13) and new client (3). But sloth's wire format ≠ autowire's, so adopting it means
  changing the **server and the old client** too — a larger, cross-cutting change that touches
  shipping code. Reasonable as a *later* convergence once we're modernizing both ends, not for the
  isolated first cut.
- (c) Hand-written/codegen client stubs — a degenerate form of (b); fine if the bespoke macro proves
  fiddly.

**Recommendation:** (b) for the isolated new-client build; keep **sloth** in mind as the eventual
unifying target if/when the server modernizes. Flag this as a decision to confirm before the API/IO
wrapper (doc 10's "transport seam") is built — the seam and the RPC choice are the same piece.

## Other shared-code complications
- **upickle derivation under Scala 3.** upickle 4.x supports Scala 3 (derivation via
  `ReadWriter.derived` / `macroRW`). The shared API types use `macroRW` in companions. Under the 3
  cross-build these must compile with upickle_3 — usually a drop-in, but **verify each `macroRW`
  site**; some sealed-trait/`.type` singleton patterns (e.g. `SecurityLevel`, `PropertyChange` in
  docs 05/06) may need the Scala 3 derivation form. This is per-file porting cost, bounded by the
  number of pickled types.
- **The QL / Wikitext parser** (`querki.qtext`, `querki.ql`) uses **fastparse** and
  **scala-parser-combinators**. Both have Scala 3 builds, but fastparse's macro-based combinators and
  some parser syntax changed across versions — expect real (bounded) porting work here, and it's the
  most algorithm-heavy shared code (so also the place targeted unit tests are warranted, per doc 10).
- **Source-level 2.13↔3 differences** in shared: implicit/`given` syntax stays as `implicit` (fine),
  but watch for `_`-as-wildcard-type, symbol-literal, `do`/`then`/`enum` as identifiers, auto-tupling,
  and `-Xfatal-warnings` interactions (on in this build). `-Xsource:3` on the 2.13 side minimizes
  these — another reason step 1 (→2.13) matters.
- **`scalatags`** (shared HTML, used server-side and in the old client) has Scala 3 builds; the new
  client will mostly use Calico's DSL instead, but shared scalatags usage must still compile under 3.

## Dependency readiness (Scala 3) — quick status
| Dep | Role | Scala 3? | Notes |
|---|---|---|---|
| Cats Effect / fs2 / Calico | new client stack | ✅ native | 3 is primary target |
| upickle | pickling (shared + client) | ✅ | verify `macroRW` sites under 3 |
| scalatags | shared/old-client HTML | ✅ | client prefers Calico DSL |
| fastparse | QL/wikitext parser (shared) | ✅ | macro/syntax porting cost |
| scala-parser-combinators | parser (shared JVM) | ✅ | fine |
| **autowire** | RPC (client+server) | ❌ | **the blocker — see above** |
| munit / munit-cats-effect | tests (doc 10) | ✅ | 3-native |
| scalarx | old-client reactivity | n/a | dropped (Calico replaces it, doc 03) |

## Testing interaction (doc 10)
- The new client's scenario tests run on **Scala 3 + Scala.js + jsdom** — all 3-native (munit,
  munit-cats-effect, scala-js-env-jsdom-nodejs). No 3 obstacle there.
- **Sharing the scenario/DSL vocabulary with the JVM mid-tests** (doc 10, open decision #2) is
  constrained by the same cross-version rules: shared test helpers would need to live in a
  cross-built (2.13+3) module and avoid 2-only macros. Feasible if we want it, but it's a reason the
  first cut may keep the two suites' DSLs parallel-but-separate rather than literally shared.

## Recommended decision checkpoints (before building)
1. Confirm the **2.13 stepping-stone** for shared+server (vs. attempting 2.12↔3 cross-build, which
   is the painful path).
2. Confirm the **RPC approach** — bespoke-matching-autowire (b, recommended) vs. sloth (a).
3. Decide whether shared test/DSL code is **shared or parallel** between JVM and JS suites.
