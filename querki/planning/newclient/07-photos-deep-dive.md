# Photos Package — Deep Dive

Detailed inventory of `querki/scalajs/src/main/scala/querki/photos/` (6 files, ~465 lines) plus the
`org.querki.facades.fileupload` facade (~151 lines): photo **upload**, **thumbnail display**,
**full-size viewing**, and the **carousel** for photo lists.

Read alongside:
- `01-architecture.md` §4 (server-HTML hooking) — *every* photos gadget hooks server-rendered HTML;
  there are no client-built photo pages.
- `03-dependencies.md` — photos is the **single most dependency-risky area**: the entire
  blueimp jQuery-File-Upload chain (+ load-image / canvas-to-blob) lives here, all unmaintained.
- `07` has **no shared `*Functions` trait** — it is the one feature that bypasses autowire entirely
  (see §1).

---

## 0. Shape of the package (read this first)

Photos are **Property values** (a Thing has a photo Property whose value is an image). There is no
photos "page" — instead, the server renders photo-related markup (edit buttons, thumbnails, photo
lists) inline wherever a photo property appears, and the client hooks five CSS classes to bring them
to life:

| Selector | Gadget | Role |
|---|---|---|
| `._photoEdit` | `PhotoInputButton` | The "Add / Replace Photo" button + the actual file-upload machinery. |
| `._photoThumbnail` | `Thumbnail` | A clickable thumbnail; click → show full size. |
| `._photoTarget` | `PhotoTarget` | An on-page `<img>` slot that displays the full photo when a thumbnail is clicked. |
| `._photoList` | `PhotoList` (a *hook*, not a Gadget) | Transforms a server `<div>` of images into a Bootstrap carousel. |
| (built on demand) | `ViewPhotoDialog` | A Bootstrap modal for full-size viewing when there's no on-page target. |

All registration is in `PhotosEcot.postInit`. The Ecot also owns the only cross-gadget state.

**Two halves for planning purposes:**
- **Read/display path** (`Thumbnail`, `PhotoTarget`, `ViewPhotoDialog`, `PhotoList`): pure
  presentation of server-rendered markup; no upload, no autowire. Can ship with the **read-only
  viewing** milestone (doc 04 #4).
- **Write/upload path** (`PhotoInputButton` + the FileUpload facade): the file-upload mechanism;
  depends on the input-gadget/edit-markup infra, so it belongs with the **editing** milestone — and
  carries the heavy dependency-replacement work.

---

## 1. Server communication — the odd one out (no autowire)

Unlike every other feature, photos does **not** use autowire RPC. Upload is a raw streaming HTTP
POST, issued by the blueimp plugin, to a dedicated Play route:

- Route: `POST /u/:userName/:spaceId/_photoUpload` → `PhotoController.upload(userName, spaceId,
  thingId)`. The client builds this URL via the low-level comm layer
  (`controllers.PhotoController.upload(userName, spaceId, thing).url`) and **appends `propId` as a
  query string** (`+ "&propId=" + propId`) — explicitly flagged as a "stupid" awkward signature to
  fix (make `propId` a real parameter).
- **Request**: the file bytes, streamed (`.multipart(false)` — non-multipart/HTTP-PUT-style), max
  5 MB (`maxFileSize`). **Client-side image resize is deliberately disabled in all cases**
  (`disableImageResize(true)`) — see §3 for why; resizing happens server-side.
- **Server side** (`PhotoController`, for context — not part of the client rewrite): a streaming
  body parser (`uploadBodyChunks`) spins up a `PhotoUploadActor` via `BeginProcessingPhoto`, streams
  chunks to it, then `ProcessUpload` finishes and returns `PhotoInfo(wikitext)`. The HTTP response
  body is the **upickle-pickled `Wikitext`** of the freshly-rendered thumbnail.
- **Response handling**: client `read[Wikitext](data.result)`, renders it as a `RawSpan`, and
  splices it into the DOM before the input button.

There is also a generic `POST /_upload → ClientController.upload(targetActorPath)` route (streaming
infra shared with imports etc.); photos uses the `_photoUpload` route specifically.

**Porting note:** the server contract is simple and stable (POST image bytes, get back pickled
`Wikitext`). The new client can keep hitting the same `_photoUpload` route — but the *transport*
(the blueimp plugin) must be replaced (see §5). Making `propId` a real path/form parameter is a
small, additive server change in the spirit of doc 04's isolation strategy.

---

## 2. `PhotoInputButton` (`._photoEdit`, 178 lines) — the upload control

A `HookedGadget` wrapping a server-rendered button element. Reads its context from the parent's
`data-*` attributes: `propid`, `collid`, `thing`. `isSingleton` (ExactlyOne/Optional collection)
decides replace-vs-add semantics; `hasExistingPhotos` checks for existing thumbnails.

`hook()`:
- Appends a vertical-align helper span (a CSS hack), creates the real (hidden) file `<input>` via
  `addRealInputButton()`, sets a tooltip ("Click to add/replace a photo"), shows the ready icon.

`addRealInputButton()` is the core:
- Creates `input(type=file, accept="image/*;capture=camera")` — note `capture=camera` enables direct
  camera capture on mobile.
- Wires a click-transfer: clicking the visible button triggers the hidden file input *only if* a
  `data-ready` flag is set (debounces against concurrent uploads).
- Calls `.fileupload(FileUploadOptions...)` with: the upload URL (see §1), `multipart(false)`,
  `maxFileSize(5MB)`, `disableImageResize(true)`, and callbacks:
  - `processstart` → show spinner icon (`showRunning`).
  - `progress` → compute percent (currently just `println`s — TODO whether it's useful).
  - `done` → if singleton & has existing photo, remove the old thumbnail frame; read the returned
    pickled `Wikitext`, render as `RawSpan`, insert before the button, `hookPendingGadgets()`;
    **then remove the file input and recreate it** (`addRealInputButton()` again) — an explicit
    one-shot workaround for a bug where it "only works once" otherwise; finally `showReady()`.
- A `change` handler validates the file is an image (`Pattern.matches("image.*", file.type)`),
  else shows a StatusLine error. (It also constructs a `FileReader` whose `onload` is currently a
  no-op — dead/vestigial code.)

Uses a small `FileTarget` js facade hack to read `EventTarget.files` (a scala-js-dom gap; there's a
TODO to upstream it).

**Porting note:** this gadget is a tangle of imperative jQuery + the blueimp plugin + DOM splicing,
with two acknowledged hacks (the one-shot recreate, the vertical-align helper). It's a prime
candidate for a clean rewrite as a Calico component with its own upload `IO` and a `Signal`-driven
ready/running/done state.

---

## 3. Why client-side resize is off (and a dependency consequence)

The code disables `disableImageResize(true)` unconditionally. The comment explains: it was
originally disabled only for Opera / old Android (known-unreliable), but it also fails erratically
for Safari on Mac; **server-side resizing works better**, with `maxFileSize` (5 MB) as the abuse
guard.

**Consequence for the rewrite:** the `load-image.min.js` and `canvas-to-blob.min.js`
`jsDependencies` (doc 03) exist *only* to support the plugin's client-side resize/preview — which is
turned off. They are very likely **dead weight today** and can almost certainly be dropped entirely
in the new client (confirm there's no preview-rendering dependency). This simplifies the
dependency-replacement story considerably: we need an upload transport, **not** a client-side
image-processing stack.

---

## 4. The display gadgets (read path)

- **`Thumbnail`** (`._photoThumbnail`) — a `HookedGadget[img]`. Reads `data-fullsrc`,
  `data-fullwidth`, `data-fullheight`, `data-fromprop`. On click, `showFull()`: if the page has a
  `PhotoTarget` for this property, display into it; otherwise pop a `ViewPhotoDialog`. Bootstrap
  tooltip "Click to see full sized".
- **`PhotoTarget`** (`._photoTarget`) — a `HookedGadget[img]` slot. On hook, registers itself with
  the Ecot (`recordTarget`) keyed by its `data-fromprop`. `displayFrom(thumbnail)` swaps its `src`
  to the thumbnail's full-size image. This is how a page can have a big "current photo" area that
  updates when you click thumbnails.
- **`ViewPhotoDialog`** — a Bootstrap **modal** built on demand (and cached in page metadata). Sizes
  itself to the image's full dimensions (lots of hardcoded pixel math — TODO to do via CSS) and
  shows via `.modal(ModalCommand.show)`.
- **`PhotoList`** (`._photoList`) — **a hook function, not a Gadget**. Takes a server-rendered
  `<div>` of `.item` images and *massively mutates* it into a Bootstrap **carousel**: detaches the
  images, builds carousel scaffolding (random id, indicators, prev/next arrows if >1), inserts it,
  and re-appends the images as slides. Intentionally non-auto-animating (per a user poll linked in
  the code).

**Porting note:** all four are display-only transforms of server HTML. `Thumbnail`/`PhotoTarget`/
`ViewPhotoDialog` are straightforward. `PhotoList` (carousel) and `ViewPhotoDialog` (modal) need
Calico-friendly Bootstrap-equivalents (doc 03). Because these are read-path, they can land with the
read-only viewing milestone independent of the upload work.

---

## 5. `PhotosEcot` / `PhotosInternal` — per-page coordination

The only stateful glue. Because the `Gadgets` registry is global (arch §4), per-page photo state is
stashed in **`Page` metadata** (`storeMetadata`/`getMetadata`, the unstructured per-page bag):
- `recordTarget(target)` — maintains a `Map[propId -> PhotoTarget]` on the page (the `targetKey`
  entry). Comment notes this only works because the client is single-threaded.
- `findTargetFor(thumbnail)` — looks up the target for the thumbnail's property on its page.
- `showInDialog(thumbnail)` — lazily builds & caches a `ViewPhotoDialog` on the page (`showDialogKey`)
  and shows it.

**Porting note:** this per-page registry exists to connect independently-hooked gadgets that have no
shared parent. In Calico, photo gadgets within a page can share a scoped `Signal`/`Ref` (a
"current full image" cell, a registry of targets) threaded through the page's component tree, rather
than reaching through global `Page` metadata. This is a cleaner model and removes the
single-threaded assumption.

---

## 6. Dependencies (the heavy part)

From `build.sbt` `jsDependencies` (doc 03), all driven by this package:
- **blueimp jQuery-File-Upload** chain: `jquery.fileupload.js` + `jquery.iframe-transport.js` +
  `jquery.fileupload-process.js` + `jquery.fileupload-image.js`. Effectively unmaintained.
- **load-image.min.js** + **canvas-to-blob.min.js** — client-side resize/preview support; **unused
  given resize is disabled** (§3); almost certainly droppable.
- **Bootstrap** modal (`ViewPhotoDialog`), carousel (`PhotoList`), tooltip (`Thumbnail`,
  `PhotoInputButton`) — via `org.querki.facades.bootstrap`.
- **jQuery** throughout.
- The `org.querki.facades.fileupload` facade (in-tree) — a thin, partial wrapper over the plugin's
  options/callbacks; dies with the plugin.

**The replacement** is the modern File API + `fetch`/XHR: a file `<input>` (or drag-drop) →
`File`/`Blob` → `fetch(uploadUrl, { method: POST, body: file })` (or XHR for upload `progress`
events, which `fetch` still doesn't expose cleanly) → read pickled `Wikitext` from the response. No
plugin, no iframe transport, no client-side image lib. This is well-trodden ground with Cats Effect
(`IO` around the fetch/XHR) and removes the single biggest cluster of dead JS in the client.

---

## 7. Data flows (summary)

**Display:** server renders thumbnails (`._photoThumbnail`) and optionally a target (`._photoTarget`)
and/or a photo list (`._photoList`). Client hooks them. Click a thumbnail → swap a `PhotoTarget`'s
`src`, or open `ViewPhotoDialog`. A `._photoList` becomes a carousel on hook.

**Upload:** server renders the edit control containing a `._photoEdit` button with `data-propid/
collid/thing`. Client hooks `PhotoInputButton`, builds a hidden file input wired to the blueimp
plugin pointed at `_photoUpload?...&propId=`. User picks/captures an image → streamed POST (no client
resize, 5 MB cap) → server processes + resizes + returns pickled `Wikitext` of the new thumbnail →
client renders it (`RawSpan`) into place and re-arms the input. Singletons replace the prior
thumbnail first.

---

## 8. Calico migration concerns specific to photos

1. **Upload transport is the main work** — replace the entire blueimp chain with File API +
   `fetch`/XHR (XHR if we want progress). Keep the existing `_photoUpload` route; response stays
   pickled `Wikitext`. (§6)
2. **Likely dependency wins**: drop `load-image` + `canvas-to-blob` (client resize is off — §3), and
   the `fileupload` facade. Net reduction in JS surface.
3. **All gadgets hook server HTML** — fits the hybrid/hooking model (doc 04). Display gadgets can
   ship with read-only viewing; the upload button with editing.
4. **Per-page coordination** (target map + dialog cache in `Page` metadata) → page-scoped
   `Signal`/`Ref` threaded through the component tree; removes the single-threaded assumption. (§5)
5. **Bootstrap modal + carousel + tooltip** need Calico-friendly equivalents (shared with other
   packages' Bootstrap needs — doc 03).
6. **Retire the hacks**: the "recreate the input after each upload" one-shot workaround, the
   vertical-align helper span, the hardcoded modal pixel math, the vestigial `FileReader.onload`
   no-op, and the `propId`-as-query-string. A clean component makes these unnecessary.
7. **`FileTarget`/`EventTarget.files`** facade hack — modern scalajs-dom exposes file inputs
   properly; the hack should be unnecessary.
8. **`capture=camera`** on the file input (mobile camera capture) is a small but real UX feature to
   preserve.

---

## 9. Suggested sub-ordering for the photos work

Split across two milestones:

**With read-only viewing (doc 04 #4–5):**
1. `Thumbnail` + `PhotoTarget` + `ViewPhotoDialog` — display + click-to-zoom (needs a modal
   component).
2. `PhotoList` carousel — needs a carousel component; otherwise self-contained.

**With editing (after the input-gadget/edit-markup infra):**
3. The **upload transport** (File API + fetch/XHR + progress) hitting `_photoUpload`, returning
   pickled `Wikitext`. Build/verify this in isolation first.
4. The `PhotoInputButton` equivalent — a clean Calico component over the transport, with
   ready/running/done state, replace-vs-add for singletons, and the inline thumbnail insert.
5. Confirm `load-image`/`canvas-to-blob` are unused and drop them; consider the `propId` server
   parameter cleanup.
