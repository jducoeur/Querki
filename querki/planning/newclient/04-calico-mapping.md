# Mapping to Calico — First-Pass Thinking

This is the bridge between the inventory (docs 01–03) and the eventual step-by-step implementation
plan. It is **exploratory**, not decided. It records how current concepts plausibly map onto
Calico / Cats Effect / fs2, the hard problems, and a candidate ordering.

Calico primer (for reference): a Calico component is a `Resource[IO, dom.Node]`. The DSL
(`calico.html`) builds nodes; reactive content comes from fs2 `Signal[IO, A]` rendered via methods
like `.renderable` / signal-valued children/attrs. State is `SignallingRef[IO, A]`. Lifecycle and
cleanup are tied to the `Resource` scope — so "unmount" automatically releases subscriptions,
listeners, timers. This is the structural win over Scala.Rx (no `Ctx.Owner` leak).

---

## Concept mapping

| Current | Calico / CE replacement | Notes |
|---|---|---|
| `Gadget` / `ManagedFrag` (single-use Scalatags wrapper w/ `elemOptRx`) | `Resource[IO, dom.Node]` (a Calico component) | The whole lifecycle concept is native to Calico. |
| `GadgetRef` + `<= / <~` (reactive holes) | signal-valued children: `someSignal.map(renderChild)` mounted as a child | This is *the* idiom shift. Most `GadgetRef` usage becomes "a child whose content is derived from a Signal." |
| `Var[T]` / `Rx{}` (Scala.Rx) | `SignallingRef[IO, T]` / `Signal` (fs2) | Direct conceptual map. `.now` → `.get` (in IO); `()=` → `.set`. |
| `Ctx.Owner` ownership (leaky) | `Resource` scope | Cleanup is automatic; the leak disappears. |
| `Ecology` / `ClientEcot` / `interface[X]` (service locator) | An explicit environment record threaded as a value, built once in a `Resource` (or `Reader`-ish) | Don't reproduce the registry; pass a small `AppContext` (DataAccess, Router, StatusLine, ApiClient, etc.). Some may be `IO`-returning services. |
| `PageManager` hash router | A hash router: `Signal[IO, Route]` derived from a `fs2.Stream` of `hashchange` events; the app renders `route.map(renderPage)` | Keep the `#!name?params` URL scheme for compatibility. Likely hand-rolled (small) to match exactly. |
| `Page.pageContent: Future[PageContents]` | `def page(...): Resource[IO, dom.Node]` that does its loads in `IO` | `Future` → `IO`. Async loads become `IO` in the component's setup. |
| `Client[XFunctions].foo().call(): Future` | thin `IO` wrapper over autowire `.call()` | `IO.fromFuture`. Keep autowire/upickle. |
| `StatusLine` (mutable toast) | a `SignallingRef[IO, Option[StatusMsg]]` rendered globally | Straightforward. |
| `DataAccess` mutable globals (`mainThing`, etc.) | explicit state in the env / passed down | Good chance to fix the "horrible side-effecty `setThing`" the code complains about. |
| Server-HTML hooking (`Gadgets.createGadgets`, `HookedGadget`) | **The hard problem — see below.** | No clean native answer; needs a design. |
| Scalatags construction | Calico `html` DSL | Mechanical but voluminous translation across all pages/gadgets. |
| jQuery DOM ops | scalajs-dom directly | Retire jQuery feature-by-feature. |

---

## The hard problems (call these out early)

### 1. Server-rendered HTML + class-based hooking
Querki's server emits large HTML chunks (rendered QL/Wikitext, and — critically — **edit forms**)
that the client must "bring to life" by matching CSS classes to behavior (`Gadgets.createGadgets`
→ InputGadgets that auto-save). Calico fundamentally wants to *own* its DOM via `Resource`-managed
nodes; here a big subtree arrives as an opaque `innerHTML` string.

Options to evaluate:
- **(a) Keep the hook model.** After inserting server HTML, scan for known classes and mount Calico
  components *onto/around* those existing elements. Calico can manage a node it didn't create, but
  this fights the grain and the lifecycle story gets murky (who owns the node?).
- **(b) Move rendering to the client.** Have the server return *structured data* (it largely can —
  e.g. `getOnePropertyEditor` returns editor descriptors) and render editors client-side as real
  Calico components. Cleaner long-term, but a bigger server-coupling change and more work.
- **(c) Hybrid.** Keep server HTML for read-only display content (QText/Wikitext — low interaction,
  just needs link-rewriting), but render *interactive* content (edit forms) client-side from data.
  This is probably the sweet spot, and matches where the value is.

**This decision gates the editing rewrite** and should be made before the editing milestone (it's
the biggest single feature). Display-only QText can ship much earlier with approach (a)-lite (insert
HTML, rewrite links, no real hooking needed).

### 2. The bootstrap handshake
Currently the Twirl page injects `clientRoutes` + pickled `RequestInfo` and calls
`QuerkiClient.start()` / `pageManager.setRoot()`. The new client (a Calico `IOWebApp`) needs an
equivalent: read the pickled `RequestInfo` (from a global var or a `<script type=application/json>`
the server emits), get the routes table (or replace `clientRoutes` with a typed/known route set),
and mount into the root element. Decide whether to keep the exact Twirl contract (lower risk,
parallel-deployable) or define a new entry page for the new client.

### 3. Tag autocomplete, file upload, trees, dates
The unmaintained-JS replacements (doc 03). Each is a mini-project. Tag autocomplete (MarcoPolo) and
photo upload (jQuery-FileUpload) are the two that carry real UX weight and no trivial swap.

### 4. autowire over IO + the `ResponseWrapper`/error envelope
The current `ClientImpl` does important cross-cutting work on *every* call: unwrap
`ResponseWrapper` (sync current user), and `translateServerException` (412→hard reload, 504/5xx→
status, else typed `ApiException`). The new `IO`-based API client must preserve all of this — build
it as a single wrapper service early, since every feature depends on it.

---

## Candidate milestone ordering (rough, to refine into the real plan)

The guiding principle: stand up the skeleton + the cross-cutting services first, then ship
read-only viewing, then the interactive features in increasing order of difficulty.

0. **Project scaffolding.** New sbt sub-project (`querkiClient2`?) depending on `querkiSharedJs`,
   pulling in Calico/CE/fs2. Gets a trivial Calico app rendering "hello" into a root element,
   deployable in parallel with the old client behind a flag/route.
1. **Bootstrap + env.** The handshake (read pickled `RequestInfo`), build the `AppContext`, mount
   into root.
2. **API client.** The `IO` wrapper over autowire with the full `ResponseWrapper`/error-envelope
   behavior. + `StatusLine` as a global signal.
3. **Router + chrome.** Hash router (`#!name?params`), `MenuBar`/footer shell, `Page` equivalent
   (a `Route => Resource[IO, dom.Node]`).
4. **Read-only viewing.** `ThingPage` display path: `getThingInfo` + `getThingPage`, render the
   server Wikitext HTML (QText) with link-rewriting. No editing yet. This proves the
   server-HTML-display story end to end and is genuinely useful.
5. **Index / Space list / simple pages.** IndexPage, InfoPage, ViewPage, account, etc. — mostly
   data-in, render-out, low interactivity.
6. **Decision point: server-HTML hooking strategy** (problem #1 above). Prototype before committing
   to editing.
7. **Editing.** Input gadgets + auto-save + the Model Designer. The big one; depends on #6 and on
   the tag-autocomplete replacement.
8. **The rest, by area:** conversations, security/sharing, history, search, notifications,
   publication, apps, photos (upload), admin, console.

Each milestone should be independently demoable in the parallel client.

---

## Open questions to resolve with the user before/while planning implementation
- Keep the exact Twirl bootstrap contract, or define a fresh entry page for the new client?
- Server-HTML hooking: pursue the hybrid (client-renders-interactive) direction, or preserve the
  hook model initially to minimize server coupling?
- Bootstrap 3 + jQuery: confirm "keep transitionally, retire feature-by-feature" is the intended
  posture (vs. a clean-CSS rewrite up front).
- How much should the new client honor the existing functional-test DOM contracts
  (`#_pageRendered`, `_spaceLink`, ids/classes the Selenium tests assert), vs. rewriting those tests
  alongside?
- Naming/placement of the new sbt project and its URL route for parallel deployment.
