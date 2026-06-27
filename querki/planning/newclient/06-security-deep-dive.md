# Security Package — Deep Dive

Detailed inventory of `querki/scalajs/src/main/scala/querki/security/` (16 files, ~1832 lines): the
two security/sharing UIs — the per-Thing **permissions grid** (`SecurityPage`) and the tabbed
**member / invitation / custom-role management** UI (`SharingPage`) — plus the reusable
list-editing and "saveable" machinery they introduce.

Read alongside:
- `05-editing-deep-dive.md` — security leans **heavily** on the editing infra (`propPath`,
  `InputGadget` auto-save, `getOnePropertyEditor`-rendered editors, `MultiplePropertyChanges`).
- `01-architecture.md` §4–5 (Gadget/hooking + InputGadget).
- `03-dependencies.md` — security is the main user of the **Manifest/MarcoPolo** jQuery plugin
  (flagged unmaintained) and Bootstrap popovers/tabs/dialogs.
- Shared API trait `scala/shared/.../querki/security/SecurityFunctions.scala`.

---

## 0. Shape of the package (read this first)

There are **two largely independent UIs**, both registered as pages in `PagesEcot` (there is *no*
client-side `SecurityEcot`/ecology interface — the API trait `SecurityFunctions` is the whole
contract):

| Route | Page | Param | Purpose |
|---|---|---|---|
| `_security` | `SecurityPage` | `thingId` | Per-Thing **permission grid** (who can read/edit/etc. this Thing, its Instances). |
| `_sharing` | `SharingPage` | — | Space-level **member / invitation / custom-role** management, tabbed. |

`SecurityPage` is a fairly self-contained permission editor. `SharingPage` is a tabbed hub
(`Invites`, `Members`, and — for Advanced users — `Roles`) and pulls in the bulk of the package's
files. They share the `SecurityFunctions` API and some data types, but almost no UI code.

Two cross-cutting reusable abstractions are introduced here and worth calling out for the rewrite:
- **`Saveables`** — a tiny typeclass framework for "things that can emit a `PropertyChange`",
  letting a panel batch several inputs into one `MultiplePropertyChanges`. Used by `EditRolePanel`
  and `EditShareableInvite`.
- **`ItemList`** (`ItemListManager` / `OneItemGadget` / `EditCompleter`) — a generic "list of
  clickable, editable items with a Create button" pattern. Used for both Custom Roles and Shareable
  Invitations. A strong candidate to generalize as a Calico component.

---

## 1. API surface (`SecurityFunctions` + borrowed editing/thing calls)

`SecurityFunctions`:
- **`getSecurityInfo()`** → `SpaceSecurityInfo(fromEmail, defaultRoles)` — for the invite form.
- **`getRoles()`** → `(standardRoles, customRoles)` — both `Seq[ThingInfo]`.
- **`getMembers()`** → `(members, invitees)` — both `Seq[PersonInfo]`.
- **`getMyInfo()`** → `Option[PersonInfo]` *(declared; not obviously used in the files read —
  verify).* 
- **`invite(emails, collabs)`** → `InviteResponse(newInvites, resends)`.
- **`archiveThisSpace()`** → `Boolean` (owner-only; never returns false — throws). *Invoked from
  outside this package — see `pages/AdvancedPage` per doc 02.*
- **`permsFor(thing)`** → `ThingPermissions(perms, instancePermThing, instancePerms)` (creates the
  Instance Permissions Thing on demand for Space/Model).
- **`getOnePerm(id)`** → `PermInfo`; **`getAllPerms()`** → `Seq[PermInfo]`.
- **`getSharedLinksForRole(roleId)`** → `Seq[SharedLinkInfo]`; **`getOneSharedLink(linkId)`** →
  `SharedLinkInfo`; **`getSharedLinkURL(link)`** → `String`.
- **`removeFromSpace(people)`** → `Boolean` (marks Persons removed, doesn't delete).

Note several take/return **`TOID`** (typed OID) rather than `TID` (`getSharedLinksForRole`,
`getOneSharedLink`, `getSharedLinkURL`, `SharedLinkInfo.forRole`); `oid2` accessors bridge.

**Borrowed from editing/thing (the important coupling):**
- `EditFunctions.getOnePropertyEditor(thingId, propId)` — to render the **custom** permission editor
  and the invite-text editor as server HTML (`RawDiv`).
- `EditFunctions.removeProperty(thingId, propId)` — used to mean "**inherit**" (delete the perm).
- `EditFunctions.create(model, changes)` — create a custom Role (`customRoleModel`) or a shareable
  invite (`sharedInviteModel`).
- `InputGadget.doSaveChange(thingId, MultiplePropertyChanges(...))` — batch-save role/invite edits.
- `ThingFunctions.getProperties(role)` — fetch a role's current property values (for the perm
  checkboxes); `ThingFunctions.getThingInfo` — re-fetch after save.
- `Editing.propPath(...)` — every save path is built with it (see doc 05; the shared-contract
  concern applies here too).

**Key data types** (shared, in `SecurityFunctions`): `SecurityLevel` ADT (`SecurityPublic`,
`SecurityMembers`, `SecurityOwner`, `SecurityCustom`, `SecurityInherited` — the last is
**client-only**, implied when the server sends nothing); `ThingPerm(permId, currently)`;
`ThingPermissions`; `PermInfo(id, name, isInstancePerm, summary, publicAllowed, default, appliesTo)`;
`PersonInfo(person, roles)`; `SpaceSecurityInfo`; `SharedLinkInfo(thingInfo, forRole,
requiresMembership, enabled)`; `InviteResponse`; `LinkPermsChoice`. All pickle over autowire and
carry over unchanged.

---

## 2. `SecurityPage` — the per-Thing permission grid (180 lines)

**`LevelMap` trait** (defined here, also mixed into `EditSpaceInfoPage` in the editing package):
maps `SecurityLevel` → the TID it saves as (`public`/`members`/`owner` system OIDs, plus the
sentinel strings `"custom"` and `"inherit"`). `currentPermLevel` resolves a `ThingPerm` to a level,
defaulting to the perm's `default` at Space level or `SecurityInherited` below it.

**`pageContent`:** `getThing(thingId)` + `getAllPerms()` + `permsFor(thingId)`. Determines
`isSpace`/`isModel`/`hasInstancePerms`. Renders a Bootstrap tab pair: **"This"** (perms for the
Thing itself) and, for Space/Model, **"Instances"** (the instance-permission defaults). Perms are
filtered by `appliesTo` (`appliesToSpace` / `appliesToModels` / `appliesToInstances`).

**`ShowPerms`** renders a row per `PermInfo`, each containing a **`OnePerm`**.

**`OnePerm`** (150 lines — the heart of the grid; an `InputGadget`): a radio group across the
applicable levels (Public if `publicAllowed`, Members, Owner, Custom, and Inherit unless Space).
- `currently: Var[String]` tracks the selected level OID; `isCustom`/`isInherit` derived `Rx`.
- On radio click: if **inherit**, `removeProperty` (delete the perm); else `save()` →
  `alterProperty`. While saving, it **disables the radios** via a `saving: Var[Boolean]` (chosen
  over the usual Saving/Saved toast specifically for *testability* and to avoid races — a notable
  design note).
- If **custom**, it loads a server-rendered editor for the perm via `getOnePropertyEditor` into a
  `._permCustom` area (`RawDiv` + `hookPendingGadgets`).
- `path = propPath(permInfo.id, Some(t))`; also keeps a `namePath` via `propPathOldStyleHack`
  because **the functional tests expect the old-style name** (explicit HACK comment) — relevant to
  doc 04's "honor test DOM contracts?" question.

---

## 3. `SharingPage` — tabbed sharing hub (67 lines + tabs)

`pageContent`: `getRoles()` + `getMembers()`, builds `RoleInfo` maps, then a `TabSetGadget`
(`display.TabGadget`/`TabSetGadget`) of:
1. **`InvitationTab`** — send invitations.
2. **`MembersTab`** — list/manage members.
3. **`CustomRolesTab`** — only if `SkillLevel.current == AdvancedComplexity`.

`SharingPage.Tab` is a small ADT (`Invite`/`Members`/`CustomRoles`) giving each tab a URL name; the
selected tab is persisted to the `tab` page param (`TabSetGadget` writes it on `shown.bs.tab`).

**Dependency note:** `TabGadget`/`TabSetGadget` live in `display` and are Bootstrap-tab based; each
tab's content is a `Future[Gadget]`, fired in parallel. The new client needs an equivalent tab
component.

### 3a. `RoleInfo` (11 lines)
Simple `(Map[TID,ThingInfo], Seq[ThingInfo])` wrapper; `default = roles.head`.

---

## 4. Role display & assignment (`RolesDisplay`, `StandardRoleDisplay`, `CustomRoleDisplay`)

Used in member rows and the invite form to show/change a Person's roles. `RolesDisplay` (an
`InputGadget`, path = `propPath(personRolesProp, thingId)`) composes:
- **`StandardRoleDisplay`** — a single "standard" role (a label that, on click,
  `detachReplaceWith`-swaps itself for a `<select>`; choosing one `save()`s). Saves by **slamming
  the whole value** (`curValue`).
- **`CustomRoleDisplay`** (only if custom roles exist) — a label that toggles a `CustomRoleSelector`
  dropdown of checkboxes; toggling each checkbox saves a **diff** (`AddToSet`/`RemoveFromSet`).

**Flagged inconsistency (code TODO):** the two selectors persist differently — standard slams the
full value, custom sends diffs. The rewrite should pick one model. `RolesDisplay.values` merges
both, which is fragile.

`detachReplaceWith` is an `squery` helper (to be reimplemented on scalajs-dom).

---

## 5. Members management (`MembersTab`, `PersonDisplay`)

- **`MembersTab`** — a `TabGadget`; a `table` of `PersonDisplay` rows. Tracks `personDisplays` and a
  reactive `selectedPersons`/`personsAreSelected`. A "Remove Selected Members" `ButtonGadget`
  (disabled via `Rx` until a selection exists) opens a confirm **`Dialog`** →
  `removeFromSpace(tids)` → reload.
- **`PersonDisplay`** — a `<tr>` that toggles a `selected: Var[Boolean]` on row click (with a check
  icon + Bootstrap `warning` styling), stopping child-click propagation so the inner role controls
  still work. Embeds a `RolesDisplay`. Used for both members and (in `InvitationTab`) outstanding
  invitees, differing only by `showCls`.

---

## 6. Invitations (`InvitationTab`, `EditShareableInvite`)

### `InvitationTab` (189 lines) — send email/collaborator invites
- `getSecurityInfo()` + `getOnePropertyEditor(space, inviteTextProp)` (server-rendered invite-text
  editor as `RawDiv`).
- **`InviteeInput`** and **`CollaboratorInput`** — `InputGadget`s wrapping the **Manifest** jQuery
  plugin (`.manifest(...)`, comma/enter separators). `InviteeInput` uses Manifest *without*
  MarcoPolo (just tokenized emails). `CollaboratorInput` uses **MarcoPolo autocomplete** against the
  **legacy `_getCollaborators` endpoint** — *not* an autowire call (explicit TODO to move it onto a
  real Client API). Both expose `values` via `manifestValues()`.
- A `RolesDisplay` to choose the role invitees get (seeded from `defaultRoles`).
- A **`RunButton`** ("Invite Members"/"Inviting…") → `invite(emails, collabs)`; on success reload +
  flash; handles `MaxMembersPerSpaceException`. Disabled while the user is `PendingUser` (awaiting
  their own validation), with a resend-activation prompt.
- Lists outstanding invitees as `PersonDisplay` rows (`warning` style).

### Shareable invitations (`EditShareableInvite`, 266 lines)
A different invite mechanism — shareable *links* tied to a Role:
- **`RoleInvitesList`** extends `ItemListManager[SharedLinkInfo]` (see §8); **`OneInviteGadget`**
  extends `OneItemGadget`. Each invite shows a **`ShareableInviteUrlButton`** that fetches
  `getSharedLinkURL` and pops a `Dialog` with a copy-to-clipboard field (via the deprecated
  `document.execCommand("copy")` — needs the modern Clipboard API in the rewrite).
- **`EditShareableInvite`** — the create/edit panel: a name `TextInputGadget` (with
  `NoAutoSave`+`ForProp`), two `RxCheckbox`es (`requiresMembership`, `enabled`), Save/Cancel. Save
  uses the **Saveables** machinery (§7) to build `MultiplePropertyChanges` and either
  `doSaveChange` (edit) or `create(sharedInviteModel, …)` (new), then `getOneSharedLink` to refresh,
  then `completer.editComplete`.

---

## 7. Custom roles (`CustomRolesTab`, `EditRolePanel`, `PermCheckboxes`)

- **`CustomRolesTab`** → **`CustomRoleList`** (an `ItemListManager[ThingInfo]`) of
  **`OneRoleGadget`**s.
- **`EditRolePanel`** (164 lines) — create/edit a Role. Inputs: a name `TextInputGadget`
  (`NoAutoSave`+`ForProp`) and a **`PermCheckboxes`**. Save batches via Saveables into
  `MultiplePropertyChanges`, then `doSaveChange` (edit, against `role.oid`) or
  `create(customRoleModel, …)` (new). When editing an existing role it also shows a
  `RoleInvitesList` (shareable invites for that role). Companion `prepToEdit`/`create` do the
  parallel loads (`getAllPerms`, `getProperties(role)`, `getSharedLinksForRole`).
- **`PermCheckboxes`** (78 lines) — an `InputGadget` (`NoAutoSave`+`ForProp`, prop =
  `rolePermissionsProp`) rendering a checkbox per `PermInfo` with a Bootstrap **popover** showing
  `perm.summary`. `values` = the checked perms' ids. *Caveat in code:* it matches current perms by
  **name** (from `getProperties` raw lines), not OID — explicitly flagged as "kind of sucks."

---

## 8. Reusable abstractions introduced here

### `Saveables` (`Saveables.scala`)
A typeclass `SaveablePropertyValue[T]` with `get(t): Option[PropertyChange]`, plus base traits
`SaveableBase`/`RxSaveable` and concrete `SaveableRxBoolean`, `HardcodedSaveable`. Instances exist
for `InputGadget`, `GadgetRef[Saveable]`, and any `SaveableBase`. This lets a panel write
`List(s(nameInput), s(SaveableRxBoolean(...)), s(HardcodedSaveable(...))).flatten` and wrap it in
`MultiplePropertyChanges`. **Porting note:** this is a clean little abstraction — in the new client
it becomes "a form field that can produce a `PropertyChange`," naturally expressible as a function
`IO[Option[PropertyChange]]` or a `Signal`-derived value. Keep the concept.

### `ItemList` (`ItemList.scala`) — generic editable list
- **`EditCompleter[T]`** — callback interface (`editComplete(Option[T])`).
- **`OneItemGadget[T]`** — one row: shows `displayName` as a link; on click calls `prepToEdit` →
  swaps in an editor panel (`GadgetRef` `<=`); `editComplete` swaps back, updating a `Var[T]`.
  Optional `listingButtons`.
- **`ItemListManager[T]`** — the list + a Create button that calls `prepToCreate` and shows the
  editor; `editComplete` appends the new item.

Used by both Custom Roles and Shareable Invites. **Porting note:** this is the model for a generic
Calico "editable list" component (`Signal[List[T]]` + per-row edit state). Worth building once and
reusing — it would also simplify the editing package's section lists.

---

## 9. Data flows (summary)

**View/edit Thing perms:** `_security?thingId=X` → `getThing` + `getAllPerms` + `permsFor` → grid
of `OnePerm` radios. Change a radio → either `removeProperty` (Inherit) or `alterProperty` (a level)
or load a server-rendered editor (Custom). Radios disabled during save.

**Manage members:** `_sharing` (Members tab) → `getMembers` → table of `PersonDisplay`. Change a
role inline → `RolesDisplay.save()` → `alterProperty(personRolesProp)`. Select rows + Remove →
confirm `Dialog` → `removeFromSpace` → reload.

**Send invites:** Invites tab → `getSecurityInfo` + invite-text editor → enter emails (Manifest) /
collaborators (MarcoPolo→`_getCollaborators`) + pick role → `invite(emails, collabs)` → reload+flash.

**Custom role:** Roles tab → `ItemListManager` of roles → click/Create → `EditRolePanel`
(`getAllPerms` + `getProperties` + `getSharedLinksForRole`) → edit name + `PermCheckboxes` → Save →
`MultiplePropertyChanges` via `doSaveChange` or `create(customRoleModel)`.

**Shareable invite:** within a role's panel → `RoleInvitesList` → Create/edit `EditShareableInvite`
→ Save → `create(sharedInviteModel)`/`doSaveChange` → share URL via `getSharedLinkURL` + clipboard.

---

## 10. Calico migration concerns specific to security

1. **Inherits all editing concerns** (doc 05): `propPath` shared-contract, server-rendered editors
   (`getOnePropertyEditor` → `RawDiv` for custom perms and invite text), `InputGadget` auto-save,
   `MultiplePropertyChanges` batching. The hybrid-rendering decision applies to the custom-perm and
   invite-text editors.
2. **`SecurityInherited` is a client-only sentinel** and "inherit" maps to `removeProperty`, not a
   value save. The new permission component must encode this three-way (value / custom / absent)
   model explicitly. The `OnePerm` `saving`-disables-radios pattern (chosen for test reliability)
   maps cleanly to a per-row `SignallingRef[Boolean]` gating the inputs.
3. **Two inconsistent role-save models** (slam-whole-value vs. diffs). Resolve to one in the
   rewrite; `AddToSet`/`RemoveFromSet` vs `ChangePropertyValue` both exist in the API.
4. **Manifest/MarcoPolo** (jQuery, unmaintained — doc 03) backs invitee/collaborator entry. Needs a
   replacement (token/chips input + autocomplete). Also: **`CollaboratorInput` calls the legacy
   `_getCollaborators` endpoint directly, not autowire** — the rewrite should move it onto a proper
   `SecurityFunctions`/`UserFunctions` call (a small, additive server change in the spirit of doc
   04's isolation strategy).
5. **`Saveables` and `ItemList`** are good abstractions to *carry forward* (re-expressed in CE/fs2),
   not discard. `ItemList` especially generalizes across security and editing.
6. **Browser-API modernization**: `document.execCommand("copy")` → Clipboard API;
   `detachReplaceWith`/`$.hide(ms)`/`.show(ms)` slide animations → CSS/fs2.
7. **Bootstrap dependencies**: tabs (`data-toggle=tab`, `shown.bs.tab`), popovers (`PermCheckboxes`),
   modal dialogs (`Dialog`). All need Calico-friendly equivalents or facades (doc 03).
8. **`LevelMap` is shared with the editing package** (`EditSpaceInfoPage`). Whatever models security
   levels in the new client should be shared between the security and editing milestones.
9. **`getOnePropertyEditor`-by-name matching in `PermCheckboxes`** (matching perms by display name,
   not OID) is fragile — fix opportunistically (ideally an OID-keyed server response).

---

## 11. Suggested sub-ordering for the security milestone

Depends on the editing infra (input gadgets, auto-save, `propPath`, server-editor hooking) being in
place first — so **schedule security after the editing milestone**, or at least after its shared
infra.

1. **`SecurityPage` + `OnePerm`** — the permission grid. Self-contained-ish; exercises the
   value/custom/inherit model and the borrowed `getOnePropertyEditor` editors. Good first target.
2. **The reusable abstractions** — port `Saveables` and `ItemList` as general Calico components
   (they unblock the rest and are reusable beyond security).
3. **`SharingPage` shell + tabs** — needs the new tab component.
4. **`MembersTab` + `PersonDisplay` + role display chain** — pick the single role-save model.
5. **`InvitationTab`** — needs the Manifest/MarcoPolo replacement + the `_getCollaborators` API
   cleanup. The heaviest external-dependency lift in this package.
6. **Custom roles** (`CustomRolesTab` → `EditRolePanel` → `PermCheckboxes`) and **shareable invites**
   (`EditShareableInvite`, clipboard) — built on `ItemList`/`Saveables` from step 2.
