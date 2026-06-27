# Dependencies & Libraries

What the current client depends on, what's shared/keepable, and what dies in the rewrite. Source:
`build.sbt` (`querkiClient` block ~line 204, `sharedDependencies` ~line 306) and the in-tree
`org.querki.*` packages.

## Shared (cross-platform) deps — KEEP

From `sharedDependencies` (used by both `querkiShared` JS+JVM, so the server already depends on
them — keeping them avoids server changes):
- **upickle 4.2.1** — pickling for autowire payloads and the bootstrap `RequestInfo`. Keep.
- **autowire 0.3.3** — the RPC mechanism over the `*Functions` traits. Keep (it's the API contract).
- **scalatags 0.13.1** — HTML construction. **Calico has its own DSL** (`calico.html`), also built
  on a Scalatags-like vocabulary. Shared code (server-side Twirl/HTML) keeps Scalatags; the new
  *client* will likely use Calico's DSL instead. Worth confirming how much they interop.
- **fastparse 3.1.1** — used by the shared QL/Wikitext parser (`querki.qtext`, `querki.ql`). Keep;
  not client-framework-related.

These are all lihaoyi libraries and are actively maintained — no pressure to drop them.

## Client-only deps (current `querkiClient`)

### Scala libs
- **scalarx 0.4.3** (`com.lihaoyi::scalarx`) — **DROP.** The reactive core. Replaced by Calico's
  fs2 `Signal`/`SignallingRef`. (Also the source of the known `Ctx.Owner` memory leak.)
- **scala-js-macrotask-executor 1.1.1** — the `ExecutionContext` for Futures. With Cats Effect,
  the `IORuntime` provides scheduling; this specific dep likely **drops**, though CE itself uses a
  macrotask-based executor under the hood.
- **scala-js-momentjs 0.10.3** (`ru.pavkin`) — date/time formatting facade, tied to specific
  moment.js/moment-timezone versions. Candidate to **replace** (moment is itself legacy/EOL
  upstream). Used by DateGadget / time display.
- **querki-jsext 0.12** (`org.querki`) — js facade helper macros (the author's). Probably **drop**
  (Calico/scalajs-dom cover most needs), but low-stakes; could keep transitionally.
- **jquery-facade 2.1** (`org.querki`) — the typed jQuery facade. **DROP eventually**, but this is
  load-bearing: jQuery is used pervasively (DOM manipulation, the Ajax transport, gadget hooking,
  every facade below depends on it). Removing jQuery is a large sub-project of its own; expect to
  **keep it transitionally** and retire it area-by-area.

### New deps to ADD (the Calico stack)
- **calico** (org.typelevel) — the UI framework.
- **cats-effect** — IO runtime (transitive via Calico, but we'll use it directly).
- **fs2** — streams + `Signal` (transitive via Calico).
- Possibly **http4s-dom** or plain fetch for transport if/when we retire jQuery-Ajax.
- A Calico-friendly **router** (Calico has examples; may need a small hand-rolled hash router to
  match the existing `#!name?params` scheme — see doc 04).

## In-tree homebrew libraries (`org.querki.*`) — DROP

These were vendored in-tree (the `// TODO: pull these back out to their libraries` comments). They
are the framework we're explicitly discarding:
- **`org.querki.gadgets`** — Gadget / ManagedFrag / GadgetRef / GadgetLookup / RxDiv / RxAttr.
  The entire rendering-lifecycle layer. **DROP** — Calico's `Resource[F, dom.Node]` components +
  `Signal` children replace all of it.
- **`org.querki.squery`** — `Focusable`, `Findable`, `Disableable`, `Cookies`. jQuery-flavored DOM
  helpers. **DROP / reimplement** the few useful bits (focus management, cookie access) directly on
  scalajs-dom.

## JS library facades (`org.querki.facades.*`) + `jsDependencies`

The client pulls in a stack of JS libraries via sbt `jsDependencies` (concatenated into one blob by
sbt-web-scalajs) and wraps several in typed facades. Status for the rewrite:

| Facade / JS lib | jsDependency | Used by | Verdict |
|---|---|---|---|
| **jQuery 2.2.1** | webjar | everything | Keep transitionally; long-term drop. |
| **jQuery UI 1.10.0** (custom) + touch-punch | ProvidedJS | sortable lists, Dialog, datepicker | Keep transitionally. Sortable/dialog need replacements. |
| **Bootstrap 3.3.6** (`facades/bootstrap`) | webjar | all layout/styling, modals, alerts, tabs | **Keep** — but BS3 is EOL. Consider BS5 or a CSS-only approach later. Big visual-regression surface. |
| **bootstrap-datepicker 1.6.1** (`facades/bootstrap/datepicker`) | webjar | DateGadget | Replace (native `<input type=date>` or a Calico-friendly picker). |
| **jstree 3.2.1** (`facades/jstree`) | webjar | TreeGadget / QLTree | Replace or re-facade; jstree is jQuery-bound. |
| **raty** (`facades/raty`) | ProvidedJS | RatingGadget | Replace (small; star-rating is easy native). |
| **manifest / MarcoPolo** (`facades/manifest`) | ProvidedJS (`jquery.manifest.js`) | TagSetInput / `_tagInput` autocomplete | Replace — these (jquery.manifest + MarcoPolo) are obscure/unmaintained jQuery plugins. Tag autocomplete is a core UX; needs a real plan. |
| **jQuery FileUpload** (+ iframe-transport, process, image) (`facades/fileupload`) | ProvidedJS chain | photos | **Most painful.** blueimp jQuery-File-Upload is effectively unmaintained. Replace with native `FormData`/`fetch` upload + a modern resize approach. |
| **load-image / canvas-to-blob** | ProvidedJS | photos (client-side resize) | Replace with native canvas APIs. |
| **moment / moment-timezone** | ProvidedJS | time display | moment is EOL; replace (e.g. Temporal/`Intl`, or luxon-style). |
| **jquery.autosize** | ProvidedJS | auto-growing textareas | Trivial to replace (CSS `field-sizing` or small JS). |
| **jquery.histogram** | ProvidedJS | HistogramGadget | Niche; replace or drop. |

### Key takeaways on dependencies
1. **jQuery is the spine.** Nearly every facade and the Ajax transport depend on it. The realistic
   path is to **keep jQuery available in the new client at first** and retire it feature-by-feature,
   not all at once.
2. **The unmaintained/risky stack is: jQuery-FileUpload (photos), MarcoPolo+manifest (tag
   autocomplete), jstree (trees), moment (dates), raty (ratings).** Each needs a deliberate
   replacement decision; the photos/upload chain is the heaviest.
3. **Bootstrap 3** is pervasive and EOL but low-risk to keep short-term; a CSS framework decision
   can be deferred but should be on the radar (it affects every page's markup).
4. **The keep-list is small and clean**: upickle + autowire (API), fastparse (shared parser),
   Scalatags in shared code. The new client adds the Typelevel stack and drops the homebrew +
   Scala.Rx layers.
