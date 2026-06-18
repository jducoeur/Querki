# Current Client Architecture

This describes how the existing `querki/scalajs/` client is built and how data flows through it.
Everything here is what we are *replacing*; understanding it is the point of the exercise.

File-size census (for sense of scale), by package under `scalajs/src/main/scala/querki/`:

```
display     24 files  2574 lines   <- the framework + reusable gadgets
editing      17 files  2037 lines   <- Model Designer / instance editor (biggest feature)
pages        17 files  2171 lines   <- the Page base class + concrete pages
security     16 files  1832 lines   <- sharing / roles / invitations
identity      7 files   858 lines   <- login / signup / TOS / user mgmt
photos        6 files   465 lines
conversations 6 files   440 lines
apps          4 files   416 lines
admin         6 files   399 lines
history       3 files   370 lines
datamodel     2 files   263 lines
...
```
Plus the homebrew framework libraries under `scalajs/src/main/scala/org/querki/`:
`gadgets/` (the Gadget framework), `squery/` (jQuery-ish helpers), and `facades/` (JS library
facades). These are vendored in-tree (note the `// TODO: pull these back out to their libraries`
comments in `build.sbt`) but are conceptually separate libraries the author wrote.

---

## 1. Bootstrap sequence

The client is **not** a standalone SPA that owns the page from `index.html`. It is launched *by a
Play/Twirl server page*. Sequence:

1. Play renders a server page (`client.scala.html`) which:
   - emits a `clientRoutes` JS object (Play's `javascriptRouter`) into the global namespace —
     this is the entire routing table the client can call (see comm layer below);
   - pickles the initial `RequestInfo` into a string and hands it to the client;
   - loads the compiled Scala.js (`querki-opt.js` / fullOpt) plus all the `jsDependencies`
     (jQuery, Bootstrap, jstree, fileupload, moment, etc., concatenated by sbt-web-scalajs).
2. `QuerkiClient.main` (`@JSExportTopLevel("QuerkiClient")`) runs but **deliberately does nothing** —
   the surrounding scripts may not be loaded yet.
3. The server page calls `QuerkiClient.start()`, which calls `setupEcology()`:
   - `new EcologyImpl`, then `createEcots(ecology)` constructs every Ecot (the giant list in
     `QuerkiClient.createCommonEcots`), then `ecology.init(ClientState())` runs init topologically.
4. The server page then calls exported methods to inject runtime data:
   - `dataSetting.unpickleRequest(pickled)` → populates `DataAccess.request`, sets the current user.
   - `pageManager.setImagePath(...)`.
   - `pageManager.setRoot(window, rootElement)` → this is the real "go" — it wires up the
     `hashchange` listener and does the initial navigation.

So three global JS entry points matter: `QuerkiClient.start`, `dataSetting`, `pageManager`,
`userManager` (all `@JSExport`ed off `QuerkiClient`).

**Calico implication:** Calico apps normally own a `def main` via `IOWebApp` and render into a root
element themselves. We will need an analogous "the server page injects pickled `RequestInfo` and a
root element, then we start" handshake — either keep the Twirl page contract, or move more of it
into the client. This is an early design decision (see doc 04).

---

## 2. The Ecology (homebrew DI) — to be removed

The client mirrors the server's "Ecology" pattern (the shared base lives in
`scala/shared/.../querki/ecology/`). On the client:

- `Ecology = EcologyBase[ClientState, EcotImpl]`. `ClientState` is an empty placeholder case class.
- Each feature is a **ClientEcot** (e.g. `PageManagerEcot`, `ClientDataEcot`, `EditingEcot`),
  constructed in `QuerkiClient.createCommonEcots`. There are ~25 of them.
- An Ecot declares `def implements = Set(classOf[SomeInterface])` and is reached at runtime via
  `interface[querki.foo.Foo]` (an implicit unwrap of `InterfaceWrapperBase`).
- `postInit()` / `init()` hooks run at startup (topologically). This is where gadgets register
  themselves, factories get created, etc.

This is a **service-locator / singleton registry**. Almost every class is an `EcologyMember` and
pulls collaborators via `lazy val Foo = interface[Foo]`. There is exactly one Ecology, stored in
the `QuerkiClient.ecology` "One True Pointer."

**Calico implication:** This is global mutable wiring. In Cats Effect we'd prefer either explicit
constructor passing of a small "environment" record, or a `Resource`-built context threaded through.
We do *not* need to reproduce the Ecology; we need to reproduce the *dependency graph* it encodes.
Doc 04 sketches the replacement.

---

## 3. Navigation & the Page lifecycle (`querki.display.PageManager`, `querki.pages.*`)

Navigation is **hash-based** (`#!pageName?key=val&key=val`). `PageManagerEcot` is the router.

Key state in `PageManager`:
- `_displayRoot`, `_window` — the DOM root and window, set in `setRoot`.
- `currentPageName`, `currentParams` (`ParamMap = Map[String,String]`), `_currentHash`.
- `currentPageRx: Var[Option[Page]]` — the currently-displayed page (a Scala.Rx `Var`).
- `_nextChangePromise` — lets callers `await` the next navigation completing.
- `afterPageLoads` / `beforePageLoads` — `Notifier[Page]` publishers other code subscribes to.

Navigation flow (`setRoot` → ongoing):
1. `setRoot` registers a `hashchange` handler → `invokeFromHash()`. It also does an initial
   navigate (either to `request.navigateToOpt` if the server forced one, e.g. Unsubscribe, or to
   the current hash). Before that, it may force a fresh **Terms of Service** flow
   (`Client[UserFunctions].checkTOS()`), and may set a "pending activation" flash.
2. `invokeFromHash()`:
   - first gives outstanding `InputGadgets` a chance to save (`InputGadgets.afterAllSaved`),
   - parses `#!name?params` into `(pageName, ParamMap)`,
   - calls `renderPage(pageName, paramMap)`.
3. `renderPage(name, params)` → `Pages.constructPage(name, params)` returns a `Page`.
4. `renderPage(page)`:
   - unloads the previous page (`page.unload()`),
   - builds the outer DOM: `StatusLineInternal.statusGadget`, `MenuBar`, the `page` itself, and a
     `StandardFooter`,
   - fires `beforePageLoads`, runs `page.beforeRenderInternal()`,
   - empties the root and appends the rendered page.
5. The `Page` itself renders asynchronously (it fetches its content from the server). When done it
   calls back `PageManager.onPageRendered(page)`, which fires `afterPageLoads` and completes the
   `nextChange` promise.

`Pages.constructPage` walks a list of **PageFactories** (registered in `PagesEcot.postInit`), first
match wins; if none match and we're in a Space, it falls through to a `ThingPage` (any unmatched
name is assumed to be a Thing id). This is explicitly noted as a "stupid way to do it" in the code.

### The `Page` base class (`querki/pages/Page.scala`)

A `Page` is itself a `Gadget[HTMLDivElement]`. Important members:
- `def pageContent: Future[PageContents]` — abstract; subclass produces title + content `div`,
  usually after one or more server API calls. This async-ness is the heart of every page.
- `std: StandardThings` — system Things, fetched once (`DataAccess.standardThings`), gated by
  `setStd` before render.
- `beforeRender()` / `afterRendered()` / `refresh()` — lifecycle hooks.
- `flash(isError, msg)` — Bootstrap alert at top of page, backed by a `Var[Seq[Gadget]]`.
- `reindex()` — walks the DOM assigning `tabindex` for accessibility/keyboard order. Recomputed
  on layout change.
- `inputDependencies` and `gadgetListeners` — per-page coordination buses for input gadgets.
- A `renderedContentFuture` promise that resolves when the page is fully rendered (the test
  harness keys off the resulting `#_pageRendered` span).

`Page.doRender()` builds a standard chrome: a flash area, a "viewing history" banner (driven by an
`Rx` from `History`), a space-link subtitle, and a spinner placeholder that is `replaceContents`'d
once `pageContent` resolves.

### Concrete pages / factories (`PagesEcot`)
Registered factories: `_explore`, `_view`, `_createAndEdit`, `_sharing`, `_advanced`, `_index`,
`_account`, `_createSpace`, `_importSpace`, `_security`, `_spaceInfo`, `_undelete`, `_doUnsub`,
plus the catch-all `thingPageFactory` (renders any Thing). See doc 02 for what each does.

---

## 4. The Gadget framework (`org.querki.gadgets.*`) — to be removed

This is the homebrew rendering layer, built on **Scalatags** (`scalatags.JsDom`) plus **Scala.Rx**.

- **`ManagedFrag[Output <: dom.Node]`** (`core/ManagedFrag.scala`): wraps a Scalatags `Frag`, but
  *single-use* — rendering records the resulting DOM node in `elemOptRx: Var[Option[Output]]`. It
  intercepts Scalatags' `render` (to capture the node + fire `onRendered`) and `applyTo` (to
  capture the parent + fire `onInserted`). So a Gadget knows its own DOM node and parent.
- **`Gadget[Output <: html.Element]`** extends `ManagedFrag`: subclass fills in `doRender()`
  returning Scalatags; `onCreate(elem)` hook runs post-render. `TypedGadget` wraps a raw `TypedTag`
  (there's an implicit `tag2Gadget` in `globals`, so Scalatags flows into Gadgets automatically).
- **`GadgetRef[G <: Gadget[_]]`** (`core/GadgetRef.scala`): a *reactive hole* you place in a
  Scalatags tree and fill later. `ref <= gadget` (assign) / `ref <~ gadget` (assign, keep old in
  DOM) render and splice the gadget into the live DOM. Has `mapRx`/`flatMapRx`/`whenRendered`
  helpers for reacting to the gadget becoming available. This is the main idiom for "build the
  static structure now, fill in the dynamic parts as data arrives." **This pattern is everywhere**
  and is the thing most in need of a clean Calico replacement (Calico's `Signal`/`Resource`-based
  children are the natural answer).
- **`GadgetLookup`** (`core/GadgetLookup.scala`): annotates rendered DOM nodes with a back-pointer
  to their Gadget, and walks parents to find the containing Gadget/Page (`findParentGadget`). Used
  by `Pages.findPageFor`.

### Hooking server-rendered HTML — the other half of gadgetry

A *lot* of Querki content is HTML **rendered on the server** (QL/Wikitext output, edit controls)
and shipped to the client as raw HTML strings. The client must "bring it to life" by attaching
behavior to specific CSS classes. That's what **`Gadgets` (the Ecot, `display/Gadgets.scala`)**
does:
- A registry: `Map[selector -> Seq[constructor]]`. Ecots call `registerSimpleGadget("._tree", …)`,
  `registerHook(selector)(elem => …)`, etc. during `postInit`.
- `createGadgets(root)`: after raw HTML is inserted, jQuery-finds every registered selector under
  `root` and constructs the matching gadget, binding it to the existing element (`setElem`).
- `HookedGadget` / `HookGadget`: a Gadget built *around* an already-existing DOM element rather
  than creating one. `gadgetCreated`/`hookPendingGadgets` defer the `prep()`/`hook()` step until
  after insertion (and re-run if hooking creates more gadgets, e.g. nested input gadgets).
- `QText` / `QTextSpan` / `RawDiv` / `RawSpan` (`display/QText.scala`): gadgets that take a
  `Wikitext` or raw HTML string, `raw()`-render it, then `prepContents` it — which **rewrites all
  `<a href>`s** through `QTextUtils.adjustUrl` (server URLs → client hash URLs, and same-hash links
  become reloads) and calls `Gadgets.createGadgets`.

**This server-HTML-hooking model is fundamental to Querki and must survive the rewrite**, even
though the implementation will change. The server will keep emitting HTML+classes; the new client
still has to find those classes and attach Calico-driven behavior. This is arguably the single
biggest architectural question for the Calico port (Calico wants to *own* the DOM; here the server
owns big chunks of it). See doc 04.

---

## 5. Input gadgets & auto-save (`querki.display.input.*`)

Edit controls are a specialized Gadget family. `InputGadget[T] extends HookedGadget[T]`:
- Each knows its `thingId` (from a `data-thing` attr, falling back to the page's main thing), its
  `path` (the field `name`), and its `propCollection`/`isOptional` (from `data-collid`).
- `values: Seq[String]` (abstract) — current on-screen value(s).
- `save()` builds a `ChangePropertyValue(path, values)` and calls
  `Client[EditFunctions].alterProperty(thingId, msg)`. Shows "Saving…"/"Saved" via `StatusLine`,
  surfaces `ValidationException`/`GeneralChangeFailure`, triggers `savecomplete`/`saveerror` DOM
  events.
- `beginChanges()`/auto-save coordination via `InputGadgetsEcot`: it tracks
  `gadgetsBeingEdited`, and `afterAllSaved: Future[Unit]` lets navigation block until all dirty
  inputs have flushed (called from `PageManager.invokeFromHash`).

Registered input gadgets (`InputGadgetsEcot.postInit`): `_textEdit`, `_largeTextEdit`,
`_tagSetInput`, `_tagInput` (MarcoPolo autocomplete), `.sortableList`, `select`,
`_deleteInstanceButton`, `_rating`, `_optYesNo`, `.propEditor` (catch-all; currently handles
checkboxes), `_dateInput`, `_depends` (dependent inputs), `_rxTextInput`.

So the **edit data flow** is: server emits an edit form as HTML with these classes → client hooks
each into an InputGadget → user edits → gadget auto-saves a single property change via
`EditFunctions.alterProperty` → server responds, gadget reflects success/error inline. Each field
saves independently; there's no page-level "Save" for most editing.

---

## 6. Reactivity (Scala.Rx) — to be replaced

The current client uses **`com.lihaoyi::scalarx 0.4.3`** (`import rx._`). Idioms:
- `Var[T]` mutable cells; `.now` to read, `() =` to write.
- `Rx{ … }` derived/computed signals; `.map`/`.flatMap`.
- `Ctx.Owner` ownership for cleanup — every `Page` provides `implicit val ctx = Ctx.Owner.safe()`.
  There's an acknowledged **memory leak**: `Page.unload()` can't actually kill the Owner because
  Scala.Rx 0.4.3 doesn't support it (`// ctx.kill()` is commented out).
- Reactive DOM bits: `org.querki.gadgets.components.RxDiv` / `RxTextFrag`, and
  `querki.display.rx.*` (`RxText`, `RxSelect`, `RxCheckbox`, `RxRadio`, `RxButtonGroup`,
  `RunButton`, `QuerkiEmptyable`). These bind `Rx` values to DOM that updates in place.

**Calico implication:** Calico/fs2 `Signal[F, A]` + `SignallingRef` is the direct replacement, with
proper resource-scoped cleanup (no leak). The `Ctx.Owner` lifetime problem just goes away because
Calico ties DOM subscriptions to `Resource` scopes.

---

## 7. Comm / API layer (`querki.comm.*`, `querki.client.*`, autowire)

Two layers of server communication:

### 7a. Low-level Play routes (`querki.comm`)
- `ApiCommEcot.controllers = js.Dynamic.global.clientRoutes.controllers` — the Play
  `javascriptRouter` object injected by the Twirl page. **Untyped `js.Dynamic`.**
- `PlayCall` (a `@js.native` facade) wraps one route: `.url`, `.absoluteURL`, `.method`,
  `.ajax(settings)`.
- `PlayAjax.callAjax(data*): Future[String]` adapts jQuery's deferred Ajax into a Scala `Future`.
- Convenience: `SpaceCall.spaceCall(...)` auto-injects `(userName, spaceId)` as the first two args
  of any route (a convention the server routes uniformly follow). `spaceUrl(...)` returns the URL.

This layer is used directly for things like image upload, raw URLs, and is the transport under
autowire.

### 7b. autowire RPC (`querki.client.ClientImpl`, the `*Functions` traits)
The main client↔server channel. `ClientImpl extends Client` (autowire client):
- Shared API traits live in `scala/shared/.../querki/<area>/<Area>Functions.scala`. Calling
  `Client[ThingFunctions].getThingInfo(id).call()` autowire-pickles the request (upickle) and POSTs
  it.
- `doCall(req)`: if there's a current Space, POSTs to
  `controllers.ClientController.apiRequest(userName, spaceId)`; otherwise to
  `rawApiRequest()` (Space-less calls).
- `makeCall`: attaches `RequestMetadata(querkiVersion, currentPageParams)`. While viewing History,
  it injects the history version param and *blocks* calls that aren't `isLegalDuringHistory`.
- `interceptFailures`: every response is a `ResponseWrapper(currentUser, payload)` — it updates
  `UserAccess.setUser` from every response (so the client's notion of "who am I" rides along with
  every call), then returns the payload.
- `translateServerException`: maps HTTP status → behavior:
  - **412 Precondition Failed** → client is stale → `PageManager.fullReload()` (hard reload).
  - **504** → "took too long" status message.
  - **≥500** → generic internal-error status message.
  - otherwise → unpickle an `ApiException` (the normal typed-error path) and propagate it.

**The full set of shared API traits** (each has a server `*Impl`; the client calls them via
autowire):
`CommonFunctions`, `ThingFunctions`, `EditFunctions`, `SecurityFunctions`, `ConversationFunctions`,
`PublicationFunctions`, `ImportSpaceFunctions`, `ImexportFunctions`, `AdminFunctions`,
`SearchFunctions`, `HistoryFunctions`, `GraphQLFunctions`, `NotificationFunctions`, `EmailFunctions`,
`AppsFunctions`, `ConsoleFunctions`, `UserFunctions`.

**Calico implication:** autowire + upickle can be kept as-is (they're cross-platform and not part
of the "framework" we're discarding). What changes is *how results drive the UI* (Future →
`IO`/`Signal`). We may want a thin `IO`-returning wrapper around `.call()`. The `js.Dynamic`
`clientRoutes` access and jQuery-Ajax transport are candidates for modernization but can be kept
initially to reduce risk.

---

## 8. Cross-cutting services (Ecots that aren't pages)

- **`DataAccess` / `DataSetting`** (`querki.data.ClientDataEcot`): holds `request: RequestInfo`,
  current `space`, `mainThing`, `mainModel`, and the cached `StandardThings`. `unpickleRequest`
  ingests the server-pickled bootstrap data. Convenience accessors `userName`/`spaceId`/`thingId`.
  Notes acknowledge `setThing`/`mainThing` is "horrible" side-effecty global state to be removed.
  No caching of ThingInfo yet (TODOs).
- **`StatusLine`** (`display/StatusLine.scala`): the transient status/toast bar
  (`showBriefly`/`showUntilChange`/`clear`). Used pervasively for "Saving…", errors, etc.
- **`UserAccess`** (`querki.identity.UserManager`): current user identity, login state, the
  resend-activation button, kept in sync via `setUser` on every API response.
- **`Localization`** (`querki.local`): message bundles; pages do `msg("pageTitle", …)`. Gated by
  `Localization.ready` before pages render.
- **`History`** (`querki.history`): "view this Space as of time T" mode. Adds a banner, restricts
  which API calls are legal, injects a version param into calls.
- **`Notifications`**, **`ProgressDialog`**, **`Print`**, **`SkillLevel`** (progressive-disclosure
  of UI complexity), **`QTextUtils`** (URL adjustment + wikitext helpers).
- **`Gadgets`**, **`InputGadgets`**, **`PageManager`**, **`Pages`** — the framework Ecots covered
  above.

---

## 9. Data flow summary (the one-paragraph version)

Server renders a Twirl shell that injects `clientRoutes` (Play JS routes) + a pickled
`RequestInfo`. The Scala.js client boots its Ecology, unpickles the request, and `setRoot`s into a
DOM element. Hash-based navigation picks a `Page`; the Page fires one or more **autowire** calls
(via `Client[XFunctions].foo().call()`) to fetch its data (often pre-rendered **Wikitext/HTML**),
builds a Scalatags/Gadget tree, and inserts it. Server-rendered HTML chunks are "brought to life"
by `Gadgets.createGadgets`, which matches CSS classes to registered gadget constructors —
crucially turning edit-control markup into **InputGadgets** that **auto-save** each field
independently through `EditFunctions.alterProperty`. Reactivity (live-updating bits, modal refs)
is done with **Scala.Rx** `Var`/`Rx` and the **`GadgetRef`** reactive-hole pattern. Every API
response carries the current user, keeping identity in sync.
