# Editing Package — Deep Dive

Detailed inventory of `querki/scalajs/src/main/scala/querki/editing/` (17 files, ~2037 lines): the
Model Designer, Advanced Editor, instance editors, property creation, and the per-field auto-save
machinery. This is the most complex and stateful feature in the client, and (per doc 04) the
milestone most blocked on the "server-rendered HTML vs. client-rendered" decision.

Read this alongside:
- `01-architecture.md` §5 (InputGadget/auto-save framework) and §4 (Gadget framework / server-HTML
  hooking) — editing is the heaviest user of both.
- The shared API trait `scala/shared/.../querki/editing/EditFunctions.scala`.

---

## 0. The two editing paradigms (read this first)

Querki editing is split across **two quite different mechanisms**, and the new client must handle
both:

1. **Per-field auto-save of server-rendered editors.** The server renders an edit control as an
   HTML string (`PropEditInfo.editor`, or the output of the `_edit` QL function), the client
   inserts it (`RawDiv`/`QText`) and `Gadgets.createGadgets` hooks each control into an
   **`InputGadget`** (see arch §5). Each control auto-saves *its own field* via
   `EditFunctions.alterProperty` on change. There is **no page-level Save** for these. This is how
   instance editing and the actual value-editing inside the Model Designer work.

2. **Structured client-built UI (the Model Designer).** `ModelDesignerPage` and its gadget tree
   (`PropertySection`, `PropValueEditor`, `AddPropertyGadget`, `CreateNewPropertyGadget`, …) are
   real client-side Scalatags/Rx gadgets that orchestrate **structural** edits — adding/removing
   properties, reordering instance properties (drag-and-drop), creating new properties/models,
   changing the model. These call `addPropertyAndGetEditor`, `removeProperty`, `create`,
   `getPropertyEditors`, etc. *Inside* this structured UI, the actual property *values* are still
   edited via mechanism #1 (the `editor` HTML embedded in each `PropValueEditor`).

So even the Model Designer is a **hybrid**: structured chrome built client-side, wrapping
server-rendered value editors. Doc 04's "hybrid" recommendation is essentially what already exists
here — the rewrite would formalize it and replace the jQuery/Rx plumbing.

---

## 1. Entry points & registration (`EditingEcot`, `package.scala`)

`EditingEcot` (impl of the `Editing` ecology interface) registers:

**Page factories** (hash routes → pages):
| Route | Page | Param | Purpose |
|---|---|---|---|
| `_editInstances` | `EditInstancesPage` | `modelId` | Edit all instances of a Model (QL `_edit` based). |
| `_modelDesigner` | `ModelDesignerPage` | `modelId` | Design a Model. |
| `_advancedEditor` | `ModelDesignerPage` | `thingId` | Same page class, for a non-Model Thing. |
| `_editSpaceInfo` | `EditSpaceInfoPage` | — | Simplified "edit the common Space properties" page. |

**Hooked gadgets** (CSS selector → gadget, for server-rendered HTML):
- `._advancedEditButton` → `AdvancedEditButton`
- `._pickList` → `PickListGadget`  *(legacy)*
- `._checklist` → `CheckList`  *(replacement for PickListGadget)*

(The per-field input gadgets — `_textEdit`, `_tagSetInput`, `select`, etc. — are registered by
`InputGadgetsEcot`, not here; see arch §5. Editing relies on them.)

**The `Editing` interface** also exposes:
- `propPath(propId, thingIdOpt)` / `propPath(propId)` — **builds the field `name` string** the
  server expects: `v-${propId}-${thingId}`. This is explicitly flagged as "evil magic knowledge
  that just happens to match FieldIds on the server" — there is *no shared abstraction*; the format
  is duplicated knowledge. **Recommended fix (now sanctioned — see README/doc 04 isolation note):**
  lift this format into shared code (`querkiShared`) and have *both* the server `FieldIds` and the
  existing client call it. This is an additive, confidence-increasing change — it converts a silent
  drift risk into a single source of truth, the existing client gets safer (surgical, no behavior
  change), and the new client reuses it for free. This is the canonical example of the kind of
  modest server + careful old-client change the project now permits.
- `propPathOldStyleHack(...)` — strips a leading char to cope with the server sometimes emitting raw
  OIDs without the `.` prefix. Pure tech debt; flagged to remove once the server is fixed.
- `getSomePropertyEditors(thingId, propIds*)` — fires N `getOnePropertyEditor` calls in parallel,
  returns `Map[TID, Gadget]` of `RawDiv(editor)`. Used by `EditSpaceInfoPage`.

---

## 2. API surface used (from `EditFunctions` + `ThingFunctions`)

`EditFunctions`:
- **`alterProperty(thingId, PropertyChange)`** → `PropertyChangeResponse` — the core per-field save
  (called from `InputGadget`/`CheckList`/`PickListGadget`, not directly from pages).
- **`create(modelId, Seq[PropertyChange])`** → `ThingInfo` — create Thing/Model/Property (yes,
  Properties are created via `create` against `std.core.urProp`; Models via `simpleThing` +
  `isModelProp`).
- **`getPropertyEditors(thingId)`** → `FullEditInfo` — all editors + which props are "instance
  props" + `instancePropPath` + `derivingName`.
- **`getOnePropertyEditor(thingId, propId)`** → `PropEditInfo` — one editor (may not yet be on the
  Thing).
- **`addPropertyAndGetEditor(thingId, propId)`** → `PropEditInfo` — add an existing prop + get its
  editor.
- **`removeProperty(thingId, propId)`** → `PropertyChangeResponse`.
- **`getModelType(modelId)`** → `TypeInfo` — get/create the Model Type (for "list of <Model>" props).
- **`changeModel(thingId, newModel)`** → `ThingInfo` *(invoked via `DataModel.changeModel`)*.
- **`getPropertyUsage(propId)`** → `PropUsage(nModels, nInstances)` — used to decide whether to fully
  delete a removed property.
- **`removePropertyFromAll(propId)`** → `OperationHandle` — long-running; not obviously wired in the
  files read (likely invoked from `PropertyDetails`/advanced flows; verify).
- `getUndefinedTagView(modelId)` — not seen used in client editing files.

`ThingFunctions` (used by editing): `getThingInfo`, `getNumInstances`, `evaluateQL` (the `_edit`
function!), `deleteThing`, `getPropertyDisplay` (for descriptions). Plus `DataAccess.getAllProps`,
`getAllTypes`, `getThing`.

**Key data types** (shared, in `EditFunctions` object): `PropertyChange` ADT — `ChangePropertyValue`,
`MoveListItem`, `AddListItem`, `DeleteListItem`, `AddToSet`, `RemoveFromSet`,
`MultiplePropertyChanges`; responses `PropertyChanged` / `PropertyNotChangedYet` (client-only);
`FullEditInfo`, `PropEditInfo` (carries `editor: String` raw HTML), `PropUsage`. These pickle over
autowire and **carry over unchanged** to the new client.

---

## 3. `ModelDesignerPage` (372 lines — the centerpiece)

The single most complex page. Flow:

**Construction / `beforeRender`:**
- Param is `modelId` *or* `thingId` (the page serves both Model Designer and Advanced Editor).
- Registers page-specific Bootstrap-styling hooks (`._largeTextEdit`, text inputs → `col-md-10
  form-control`). *(These hooks mutate server-rendered editor markup; a Calico rewrite that
  client-renders editors would set classes directly instead.)*
- If no model id, pops `DataModel.chooseAModel(...)` dialog; if the user picks one, `create`s a new
  Model (`isModelProp=true`) and uses its oid.

**Eager parallel loads** (kicked off at construction for responsiveness):
`allTypesFut = getAllTypes()`, `allPropsFut = getAllProps()`, then derived maps
`propMapFut / typeMapFut / collMapFut / modelMapFut` (by OID). `SpaceProps` is a **tree** (this
Space's props + recursively its Apps' props); `spacePropsRec` flattens it.

**`pageContent` assembly** (a big for-comprehension):
- `getThing(modelId)`, its model, and the space Thing.
- `getPropertyEditors(modelId)` → `FullEditInfo`; on `ModelLoopException`, shows a status message
  and degrades to empty.
- Partitions props into **instance props** vs **model props**; sorts instance props by
  `instancePropIds` order (this order is user-controlled via drag-drop and persisted).
- Builds the page: title, "Change Model" link (`DataModel.changeModel` → reload), and:
  - If a **Model**: two `PropertySection`s ("Instance Properties" — draggable/sortable — and "Model
    Properties") plus an `AddPropertyGadget`.
  - If a **non-Model Thing** (Advanced Editor): one `PropertySection` (`allProps`) + `AddPropertyGadget`.
- **Publication integration** (when `isPublishable(model)`): a publish-notes editor
  (`getOnePropertyEditor` for `publishNotesProp`) and a cluster of buttons (Publish / Update / Minor
  / Discard / Done) gated on `canPublishPerm`, wired to `Publication.publish/update/discardChanges`.
  Otherwise a plain "Done" link.

**Mutating operations on the page object:**
- `addProperty(propId, openEditor)` → `addPropertyAndGetEditor` → `instancePropSection().appendEditor`
  + `hookPendingGadgets()`.
- `removeProperty(editor)` → `removeProperty`; on success removes the editor from its section, then
  if the prop is local and now unused (`getPropertyUsage` == 0/0) `deleteThing`s it entirely.
- `PropSectionHolder` — mutable holders for the two sections (`instancePropSection`,
  `modelPropSection`).

**Porting concern:** this page is a web of mutable holders + imperative DOM splicing
(`appendEditor`, `removeEditor`, `refreshEditor` doing `$(...).replaceWith`). In Calico this becomes
a `SignallingRef[List[PropEditInfo]]` (or similar) per section, rendered reactively — the imperative
append/remove/replace all collapse into list-state updates.

---

## 4. The property-row stack: `PropertySection` → `PropValueEditor` → `PropertyDetails`/`PropertyEditor`

### `PropertySection` (a `<ul>`, *is an* `InputGadget`)
- Holds a `Var[Set[TID]] propIds` of the props currently in the section.
- **Sortable** sections use jQuery UI `.sortable` with `connectWith("._propertySection")` (drag
  between Instance and Model sections) and `handle("._dragHandle")` (touch-punch friendly). On drop
  (`stop`), it `save()`s — and `save()` here means: `values` reads the DOM child `<li>` order
  (`data-propid` of each) and `alterProperty`s the **instance-prop list/order** (path =
  `instancePropPath`). So **drag-reordering persists by saving the whole ordered list**.
- `appendEditor` / `removeEditor` (animated hide+remove) / `refreshEditor` (re-fetch one editor via
  `getOnePropertyEditor` and `$.replaceWith` the rendered gadget). Each mutates `propIds` and calls
  `updatePage()` (tab reindex).
- `doRender`: `<ul class="_propertySection form-horizontal" name=<path> data-thing=<tid>>` with a
  `PropValueEditor` per prop (or a dummy `<li>` drop target when empty).

**Porting concern:** drag-and-drop reordering is the single trickiest interaction. jQuery UI
sortable + touch-punch has no Calico equivalent; needs a deliberate replacement (HTML5 DnD, or a
small Calico-friendly sortable, or a library). The *persistence* model (save the ordered id list)
is clean and stays.

### `PropValueEditor` (a `<li>`)
- Wraps one property's value editor. `prompt`/`tooltip` from `PropEditInfo` (wikitext).
- Renders: a clickable prompt `label` (toggles details), the **server-rendered editor**
  (`new RawDiv(info.editor)`), a drag handle (if sortable), a `DeleteInstanceButton`
  (→ `page.removeProperty(this)`), and — for the Display Name prop on non-Spaces — a `DeriveNameCheck`.
- A `propDetailsArea` `GadgetRef` that toggles (slideDown/Up) between a `PropertyDetails` view and a
  `PropertyEditor` (editing the property *definition*, not its value). `detailsShown: Var[Boolean]`.
- `propEditDone()` → slide up, then `section.refreshEditor(this)` → focus the refreshed editor.

### `PropertyDetails`
- Read-only-ish details panel: a `DescriptionDiv` for the property, plus an "Edit Property" button
  (if `canEditProperty`) that swaps in the `PropertyEditor`. Fakes an `RxThingSelector` to reuse
  `DescriptionDiv`.

### `PropertyEditor`
- Edits the **property's own meta-properties**. It does `getPropertyEditors(propId)` (the property
  *is itself a Thing* with editable props) and renders a nested `PropertySection` for the property.
  "Done" → `valEditor.propEditDone()`. This is **recursion**: the property-editing UI is the same
  PropertySection machinery pointed at the property Thing.

---

## 5. Adding properties: `AddPropertyGadget` / `AddExistingPropertyGadget` / `CreateNewPropertyGadget`

A three-state gadget cluster (the most Rx-intensive code in the client):

### `AddPropertyGadget`
- A `GadgetRef[Gadget[_]] theGadget` that swaps between three states: an **init "+ Add a Property"
  button**, the **AddExisting** panel, and the **CreateNew** panel. `reset()` returns to the button.

### `AddExistingPropertyGadget`
- A dropdown (`RxSelect`) of existing, applicable, not-already-used properties. Options computed
  reactively (`propOptions: Rx`) from the `SpaceProps` tree, filtered by `appliesTo` (Kind) and by
  `existingPropIds` (a `Rx` union of both sections' `propIds`), grouped into Standard/Advanced/App
  optgroups. The "Add" button is `disabled := Rx{ selectedProperty.isEmpty }`; on click →
  `page.addProperty`. A `DescriptionDiv` shows the selected property's description live. Buttons to
  switch to Create-New or cancel.

### `CreateNewPropertyGadget` (382 lines)
The richest reactive form in the codebase. Fields: name (`RxInput` with name-filtering + blur
fixup), **Type** selector (`RxSelect`) *or* **Model** selector (mutually exclusive — selecting one
clears the other via `whenSet`/`trigger` listeners), a **Collection** button group
(`RxButtonGroup`, auto-set from the type's `preferredCollection`, or List when a Model is chosen).
For **pointer types** (Link/Tag — hardcoded check against `linkType`/`tagType`), an extra
`RxRadio` group: point to Any / an Existing model (`RxSelect`) / a New model (name `RxInput`).

- A web of derived `Rx`s: `selectedBasis` (model-or-type), `isPointerType`, `pointerModelIsLegal`,
  `pointerIsLegal`, and the **Create** button's `disabled` is a single `Rx` over name +
  collection + basis + pointer-legality.
- On Create: assembles `ChangePropertyValue`s for the meta-props (`nameProp`, `collectionProp`,
  `typeProp`, optionally `linkModelProp`); if "new model", first `create`s the model; if Model-based,
  `getModelType` first; then `create(std.core.urProp, fullProps)` and `page.addProperty(oid, true)`.
- Mixes in `SkillLevelsNeeded` — the Model selector and "or" text only show for `Advanced` users.

**Porting concern:** this is *the* showcase of the Scala.Rx → fs2-Signal translation. Dozens of
interdependent reactive cells, cross-field "clear the other" effects, validity-gated buttons. It
maps cleanly to `SignallingRef`s + derived `Signal`s, and Calico's signal-valued `disabled`/`hidden`
attrs — but it's a substantial, fiddly port and a good **proof-of-concept target** for the reactive
patterns before tackling the rest.

---

## 6. Supporting gadgets

- **`EditInstancesPage`** — edits all instances of a Model. Gets `getNumInstances`, builds a
  `PagePaginator`, then **`evaluateQL(modelId, "_edit(page, pageSize)")`** to get the instance
  editors as Wikitext/HTML, rendered via `QText`. A `CreateAnotherButton` (a `HookedGadget`) at the
  bottom: on click `create`s a new instance, `evaluateQL(oid, "_edit")` for its editor, inserts it,
  `hookPendingGadgets`, scrolls, focuses. *Self-described as "halfway" — ideally the API would
  return a list of editors instead of QL-rendered HTML.* **This page is almost entirely mechanism
  #1** (server-rendered editors), so it's a good candidate to ship early in display form but is
  fully dependent on the input-gadget hooking story.
- **`EditSpaceInfoPage`** — simplified Space-properties editor. Uses
  `getSomePropertyEditors(displayName, summary, details)` + a security `RxRadio` (Public / Members)
  with a clever `ReadSaver` `InputGadget` pattern: two hidden savers (Space + instance perms) whose
  `values` read the radio, saved together whenever the radio changes. Calls
  `SecurityFunctions.getAllPerms`/`permsFor`. Links to the full Advanced Editor and Security page.
- **`PickListGadget`** (`._pickList`, legacy) & **`CheckList`** (`._checklist`, its replacement) —
  both hook a server-rendered list of checkboxes and save **per-checkbox** via `AddToSet`/
  `RemoveFromSet` (not the whole list). Support delete-item (`deleteThing`) and "quick create" (type
  a name + Enter → `create` a new instance, add to set, reload). `CheckList` adds live text filtering
  by wiring to a separate filter `RxInput` through the page's `gadgetListeners` bus. *PickListGadget
  is dead-ish (replaced); the new client should implement only `CheckList`.*
- **`DeriveNameCheck`** — the "derive Link Name from Name" checkbox on the Display Name editor; an
  `InputGadget` whose `values` are `deriveAlways`/`deriveNever`; saving triggers a page reload
  (changes which props load).
- **`DescriptionDiv`** — watches an `RxThingSelector` and renders a live description of the selected
  property/type (collection + type links via the maps, plus `getPropertyDisplay` for summary/details
  rendered as `QText`). Reused by both Add gadgets and `PropertyDetails`.
- **`AdvancedEditButton`** (`._advancedEditButton`) — a hooked "x" button in server-rendered
  instance editors that navigates to the Advanced Editor for that Thing.

---

## 7. Data flows (summary)

**Open Model Designer:** route `_modelDesigner?modelId=X` → eager `getAllTypes`/`getAllProps` →
`getThing(model)` + `getPropertyEditors(model)` → partition into instance/model props → render two
`PropertySection`s of `PropValueEditor`s, each embedding a server-rendered value editor (`RawDiv`)
that `Gadgets.createGadgets` then hooks into auto-saving `InputGadget`s.

**Edit a value:** user changes a hooked control → `InputGadget.save()` → `alterProperty(thingId,
ChangePropertyValue(path, values))` → "Saving…/Saved" status. Independent per field.

**Reorder instance props:** drag `<li>` between sections (jQuery UI sortable) → `stop` → instance
section `save()` → `alterProperty` of the ordered prop-id list at `instancePropPath`.

**Add existing prop:** AddExisting select → `addProperty` → `addPropertyAndGetEditor` → append a new
`PropValueEditor` → hook its editor.

**Create new prop:** CreateNew form → (maybe `create` a new model) → (maybe `getModelType`) →
`create(urProp, metaProps)` → `addProperty(newOid, openEditor=true)`.

**Remove prop:** delete button → `removeProperty` → drop the editor; if local & unused
(`getPropertyUsage`) → `deleteThing`.

**Edit instances page:** `getNumInstances` → `evaluateQL("_edit(page,size)")` → `QText` → hooked
editors; "Create another" → `create` + `evaluateQL("_edit")`.

---

## 8. Calico migration concerns specific to editing

1. **Hybrid rendering is unavoidable here.** Value editors come from the server as HTML
   (`PropEditInfo.editor`, `_edit` QL output). The structural UI is client-built. The rewrite must
   either (a) keep hooking server HTML for values (lower risk, preserves the QL `_edit` path used by
   `EditInstancesPage`), or (b) change the API so the server returns structured editor *descriptors*
   and the client renders real Calico inputs. Recommendation: **start with (a)** to get editing
   working, evaluate (b) per control type later. This is the doc-04 decision point made concrete.
2. **`propPath` is an unshared server contract** (`v-<propId>-<thingId>`). Preferred: lift the format
   into `querkiShared` so the server `FieldIds`, the old client, and the new client all share one
   definition (additive, de-risks the old client too — see doc 04 isolation note). The
   `propPathOldStyleHack` should die via the matching small server fix (stop emitting raw
   `.`-less OIDs).
3. **Per-field auto-save** maps to: each input component owns an `IO` that, on change, calls the
   wrapped `alterProperty`. The `InputGadgets.afterAllSaved` "flush before navigate" guarantee
   (arch §5) must be preserved — in CE this is cleaner (track in-flight saves in a `Ref`, await on
   route change).
4. **Drag-and-drop reordering** (jQuery UI sortable + touch-punch, `connectWith` across two lists)
   has no Calico equivalent — explicit replacement decision needed. Persistence model (save ordered
   id list) is reusable.
5. **The big reactive forms** (`CreateNewPropertyGadget`, `AddExistingPropertyGadget`) are the best
   first targets to validate the Scala.Rx→Signal port: many interdependent cells, cross-field
   clearing effects, validity-gated `disabled`/`hidden`. No server-HTML entanglement, so they can be
   ported in isolation.
6. **Recursion**: property editing reuses `PropertySection` against the property-as-Thing. Whatever
   component models a "section of property editors" must be reentrant.
7. **`GadgetRef` swap-in-place** (`theGadget <= …`, `propDetailsArea <~ …`, `guts <= …`) is used for
   modal/stateful regions — these become `Signal`-selected child components.
8. **Imperative DOM ops to retire**: `$.append`/`replaceWith`/`hide(400, cb)`/`slideUp`/`slideDown`/
   `insertBefore`, jQuery `.sortable`, `data-*` reads (`data-propid`, `data-thing`, `data-collid`,
   `data-modelid`, `data-filterid`). Animations (slide/hide) need a CSS-transition or fs2-driven
   replacement.
9. **`gadgetListeners` cross-gadget bus** (CheckList ↔ filter RxInput by element id) — replace with
   shared `Signal`s passed through the component tree rather than id-based late binding.

---

## 9. Suggested sub-ordering for the editing milestone

1. Port the **input-gadget hooking + auto-save** infra first (shared with all editing; arch §5).
   Includes the `afterAllSaved` flush-on-navigate guarantee and the `alterProperty` wrapper.
2. **`EditInstancesPage`** — almost pure mechanism #1; validates the `_edit` QL + hooking path and
   the paginator. Plus `CreateAnotherButton`.
3. **`EditSpaceInfoPage`** — small, mixes `getSomePropertyEditors` + the `ReadSaver` pattern; good
   second step.
4. **`CreateNewPropertyGadget` + `AddExistingPropertyGadget`** in isolation — the reactive-form
   proof of concept (no server-HTML).
5. **`ModelDesignerPage`** full assembly: `PropertySection` (incl. the DnD replacement),
   `PropValueEditor`, `PropertyDetails`/`PropertyEditor` toggle, add/remove flows.
6. **`CheckList`** (skip the legacy `PickListGadget`), `DeriveNameCheck`, `DescriptionDiv`,
   `AdvancedEditButton`.
7. **Publication** button cluster — coordinate with the `publication` package milestone.
