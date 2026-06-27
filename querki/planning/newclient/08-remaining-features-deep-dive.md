# Remaining Feature Packages — Deep Dive

Consolidated deep-dive on the smaller feature packages not yet covered:
**conversations, notifications, search, console, history, publication, apps, identity (+ skilllevel),
admin, email**, plus the cross-cutting **datamodel** helper. Together these are ~3,300 lines.

Read alongside docs 01 (architecture), 05 (editing — several of these reuse its infra), 06
(security — publication reuses it). This doc is organized as one section per package, then a
**cross-cutting patterns** section (§12) that is the most important part for planning, and a
**milestone mapping** (§13).

---

## 1. conversations (6 files, ~440 lines)

Comment threads attached to a Thing, shown by `ConversationPane` (instantiated inside `ThingPage`).

- **API** (`ConversationFunctions`): `getConversationsFor(thingId)` → `ConversationInfo` (with
  `canComment`/`canReadComments` flags), `addComment(thingId, text, responseTo)` → `ConvNode`,
  `deleteComment(thingId, commentId)`.
- **Gadgets**: `ConversationPane` (loads convs on create, shows pane only if the user can act),
  `ConversationGadget` (one thread; flattens a `ConvNode` tree into a list — code TODO notes the
  flattening is "all wrong" and should be recursive), `CommentGadget` (one comment; renders content
  as `QText`, delete via `DeleteButton`), `ReplyGadget` (textarea + post; autosize), and
  `NewConversationGadget` (start-a-thread affordance).
- **Two construction paths**: directly (from `ConversationPane`) *and* via `fromElem` companion
  methods that hook server-rendered comment markup (`_convCommentData` etc.) — i.e. conversations
  can be embedded in server HTML and brought to life. (Not currently registered in an Ecot's
  `postInit` in the files read — the `ConversationsEcot` is 15 lines; verify how `fromElem` is
  invoked.)
- **Data flow**: open Thing → `ConversationPane.onCreate` → `getConversationsFor` → render threads
  → reply/new → `addComment` → splice returned `ConvNode` into the DOM.
- **Calico notes**: comment list/replies map cleanly to a `Signal[List[Comment]]` with append on
  post; the tree-flattening bug is worth fixing properly (recursive `CommentGadget` rendering its
  own replies). `autosize` textarea is a tiny jQuery plugin to replace. Server-HTML hooking path
  applies (doc 04).

---

## 2. notifications (4 files, ~220 lines)

The "bell" + a recent-messages page.

- **API** (`NotificationFunctions`): `numNewNotifications()`, `getRecentNotifications()`,
  `readThrough(maxId)`.
- **`NotificationsEcot`**: holds `numNotifications: Var[Int]`; `checkNotifications()` refreshes it.
  Registers an `afterPageLoads` hook to re-check after every page load **if logged in** (explicit
  TODO: replace with WebSockets push). Registers the `_notifications` page.
- **`NotifierGadget`**: the menu-bar bell; reactively shows empty vs. full (with a count badge)
  off `numNotifications`. A clean, small reactive-DOM example (and carries a TODO lamenting the
  boilerplate of "reactively choose between pre-rendered components" — exactly what Calico's
  signal-children do natively).
- **`NotificationsPage`**: lists recent notifications (`QText` headline/content), then
  `readThrough(max)` + re-check. Contains a **server-HTML link-rewriting hack** (`beforeRender`
  rewrites `#comment` → `?showComment=comment` in comment-notification links — flagged as
  "fundamentally wrong", should send structured data instead). Uses moment.js `calendar()`.
- **Calico notes**: `numNotifications` → `SignallingRef[Int]`; the bell is a trivial Calico
  component. The post-page-load polling becomes an fs2 stream (or real push later). The link-rewrite
  hack should die via a server-side change to send structured link data (additive, doc 04).

---

## 3. search (3 files, ~144 lines)

- **API** (`SearchFunctions`): `search(query)` → `Option[SearchResults]`.
- **`SearchGadget`**: the menu-bar search box; on Enter, navigates to the results page.
- **`SearchResultsPage`**: runs `search`, renders results as a `<dl>`, **boldfacing matched
  substrings** by recursing over result `positions`. Self-contained, no mutation.
- **Calico notes**: straightforward; pure data-in/render-out. Good early/simple port. The
  boldface-by-position logic carries over directly.

---

## 4. console (3 files, ~125 lines)

An in-Space command console (Programmer feature).

- **API** (`ConsoleFunctions`): `consoleCommand(cmd)` → `DisplayTextResult | ErrorResult`.
- **`ConsolePage`**: a fixed-size textarea input (prompt `"> "`), a `RunButton`, and an output div
  that appends command + result, scrolling to bottom. `fixup` preserves leading spaces/newlines as
  `&nbsp;`/`<br>`.
- **Calico notes**: simple append-only output (`SignallingRef[List[Line]]`); trivial port.

---

## 5. history (3 files, ~370 lines)

"View this Space as of a past version" + revert/undelete. **`History` is cross-cutting** (see doc
01 §7b): it gates which API calls are legal while viewing history and injects the version param.

- **API** (`HistoryFunctions`): `getHistorySummary(end, n)` → `HistorySummary`, `rollbackTo(idx)` →
  reloaded `SpaceInfo`. (`restoreDeletedThing` is called by `UndeletePage`, which lives in the
  `pages` package, not here.)
- **`HistoryEcot`** (`History` interface): holds `currentHistoryVersion`/`currentHistoryTime`;
  `viewingHistory`, `setHistoryVersion`/`clearHistoryVersion`. **`isLegalDuringHistory(req)`** is a
  hardcoded `match` on the autowire request path (allow most of `ThingFunctions` except
  `deleteThing`, all of `HistoryFunctions`/`SearchFunctions`, deny everything else) — explicitly
  flagged as "grotesque", wanting a shared annotation on the API traits instead.
- **`HistorySummaryPage`**: a paginated table of events (`getHistorySummary`); each row expands to
  reveal "View Space" (sets history version + navigates), "Revert to Here" (confirm `Dialog` →
  `rollbackTo` → set space + reload), and "Undelete" (for delete events → `undeleteFactory`). Lots
  of per-event display mapping (`EvtSummary` ADT → `SummaryDisplayInfo`); localized reasons.
- **Calico notes**: `viewingHistory` state → a shared `Signal` consulted by the API wrapper (doc 01
  §7b banner + call-gating). The `isLegalDuringHistory` path-match is a strong candidate to lift
  into shared code as an annotation/attribute on the API traits (additive, benefits both clients —
  doc 04). The expandable table rows map to per-row open/closed `Signal`s.

---

## 6. publication (3 files, ~194 lines)

Draft/publish workflow for Publishable Spaces. **Tightly coupled to security + editing.**

- **API** (`PublicationFunctions`): `publish(oid)`, `update(oid, minor)`, `discardChanges(oid)`,
  `changePublishedModels()`.
- **`PublicationEcot`** (`Publication` interface): flag predicates off `StandardThings`
  (`isPublishable`, `isPublished`, `hasUnpublishedChanges`, `spaceHasPublications`); `publish` /
  `update` / `discardChanges` (the last pops a confirm `Dialog`). Registers the `_editPublish` page.
  These are invoked from the **Model Designer's** publish button cluster (doc 05 §3).
- **`EditPublicationPage`**: toggles whether a Model's instances are Publishable (an `RxCheckbox`
  whose change saves via `InputGadget.doSaveChange` + `changePublishedModels`), and edits the
  "who can publish" permission by **reusing security's `OnePerm` + `LevelMap`** directly.
- **Calico notes**: minimal own-UI; mostly orchestration + reuse of editing/security components. Its
  port largely falls out of those two milestones. The flag-predicate helpers are pure and carry
  over.

---

## 7. apps (4 files, ~416 lines)

The "Apps" feature: a Space can be extracted into a reusable App, and Apps can be instantiated.

- **API** (`AppsFunctions`): `getExtractableModels()`, `extractApp(ids, name, summary, details)`,
  `addApp(oid)`. (`checkAppVersions` from doc 02 not seen in these files — verify.) Plus
  `UserFunctions.createSpace(name, Some(appId))` and `ThingFunctions.getChildren`/`getProperties`.
- **`AppsEcot`**: registers `_appMgmt` and `_extractApp` pages and the `_instantiateAppButton` hook
  (`UseAppGadget`). `useApp()` → confirm-name `Dialog` → `createSpace` (logging in first if needed
  via `UserAccess.loginCore`).
- **`ExtractAppPage`**: name/summary/details inputs + an **`ExtractTree`** (a **jstree** with
  checkboxes, lazily loading instances via `getChildren`) to pick Models/Pages/Instances → `extractApp`.
- **`AppManagementPage`**: lists current apps; add by OID → `addApp` → `fullReload`.
- **Calico notes**: **the second jstree user** (besides `display.TreeGadget` — doc 03; jstree is
  jQuery-bound and unmaintained). `ExtractTree` needs a real tree-with-checkboxes replacement — the
  heaviest lift in this package. The rest is dialogs + inputs.

---

## 8. identity + skilllevel (10 files, ~860 + ~195 lines)

Login, signup, TOS, invitations, and the "Complexity" (skill-level) system.

- **API**: `UserFunctions.checkTOS`, `agreeToTOS`, `createSpace`, `resendActivationEmail`,
  `validateActivationHash`, `setComplexity`; `CommonFunctions.fetchTOS`. **Plus raw-AJAX (non-
  autowire) calls** via the comm layer: `LoginController.clientlogin`, `signupStart`,
  `handleInvite2`, and `sendPasswordReset()` (URL only). (`AccountPage`, in `pages`, uses
  `accountInfo`/`changePassword`/`changeDisplayName`.)
- **`UserManagerEcot`** (`UserAccess`): the current-user state (`_user: Option[UserInfo]`, kept in
  sync by `setUser`, which also invalidates the SkillLevel cache). `loginCore()` shows a login
  `Dialog` (a `LoginLogic` helper shared with the signup page's inline login), returns a `Future`;
  `resendActivationButton`.
- **`SignUpPage`**: a big reactive form — email/password/handle/display validation as derived `Rx`s,
  a TOS panel + agree checkbox gating an enabled `RunButton`; optional inline sign-in section.
  `doSignup` is a **raw AJAX** call; handles `HandleAlreadyTakenException`/`EmailAlreadyTakenException`.
  Composable via `SignUpPage.run` (renders itself, resolves a `Future[UserInfo]`).
- **`TOSPage`**: fetch + agree to Terms; composable via `TOSPage.run` (used by `PageManager` at boot
  — doc 01 §3).
- **`HandleInvitePage`**: accept a Space invitation (`handleInvite2` raw AJAX); may force signup
  first (`requiresMembership`); then navigate (Guest-aware flash to log in/sign up).
- **`ValidateSignupPage`**: email activation (`validateActivationHash`); requires login first.
- **skilllevel** (`SkillLevelEcot`, `SkillLevel`, `SkillLevelsNeeded`, `Complexity`): the
  **progressive-disclosure** system — `EasyComplexity`/`StandardComplexity`/`AdvancedComplexity`
  ("Participant"/"Builder"/"Programmer"), each `accepts` lower levels. `current` is cached
  (invalidated on `setUser`); `changeSkillLevel()` pops a chooser `Dialog` → `setComplexity` →
  reload. **`SkillLevelsNeeded`** is a mix-in giving gadgets `userSkillLevel`/`Easy`/`Standard`/
  `Advanced` — used to gate UI in **editing** (`CreateNewPropertyGadget`) and **security**
  (`CustomRolesTab`, `SharingPage`).
- **Calico notes**: the big reactive forms (`SignUpPage`, login dialog) are like
  `CreateNewPropertyGadget` (doc 05) — good Signal-port targets. The **raw-AJAX login/signup/invite
  endpoints** are part of the "non-autowire cluster" (§12.2). `UserAccess` (current-user) and
  `SkillLevel` (current complexity) are cross-cutting **state** that should live in the new client's
  shared environment as `Signal`s, not Ecot singletons — `setUser` already drives SkillLevel
  invalidation, so model them as derived signals.

---

## 9. admin (6 files, ~399 lines)

Site-operator pages. **The clearest candidate for living outside user-downloaded code** (a code
comment in `SpaceTimingPage` says exactly this).

- **API** (`AdminFunctions`): `allUsers`, `pendingUsers`, `upgradePendingUser`, `changeUserLevel`,
  `statistics`, `monitor`, `beginSpaceTiming`, `stopSpaceTiming`, `getTimedSpaces`,
  `getSpaceTimingsSince`. Errors surface `NotAnAdminException`.
- **`AdminEcot`**: registers `_adminStats`, `_manageUsers`, `_monitor`, `_spacesTiming`,
  `_spaceTiming`.
- **`ManageUsersPage`**: pending-user upgrade + an all-users table with per-row level `RxSelect`
  (Superadmin can also set Admin). **`MonitorPage`**: cluster state + active spaces.
  **`StatisticsPage`**: site stats. **`SpaceTimingPage`/`SpacesTimingPage`**: a profiling tool that
  **polls the server every 1s** via `setTimeout` for timing messages (the main example of client
  polling in the codebase).
- **Calico notes**: low priority, possibly a *separate* app (per doc 04's open question). The 1s
  `setTimeout` polling → an fs2 stream with proper cancellation (the current code manually tracks/
  clears a `SetTimeoutHandle`). Otherwise tables + dialogs.

---

## 10. email (1 file, ~66 lines)

- **API** (`EmailFunctions`): `getUnsubOptionsFor(payload)`, `unsubscribe(notifier, identity,
  unsubId, context)` → `Wikitext` result message.
- **`UnsubscribePage`**: the server often force-navigates here at boot (doc 01 §3,
  `request.navigateToOpt`). Reads an opaque server `payloadOpt`, lists unsub options as table rows
  with per-row buttons that call `unsubscribe` and show the result reactively (`RxDiv`).
- **Calico notes**: tiny; per-row result `Signal`. Straightforward.

---

## 11. datamodel (2 files, ~263 lines) — cross-cutting helper

Not a feature page; a shared helper Ecot (`DataModel`) used by **editing** (Model Designer),
**ThingPage**, and **AdvancedPage**.

- **API used**: `ThingFunctions.deleteThing`, `getNumInstances`; `EditFunctions.getPropertyUsage`,
  `removePropertyFromAll`, `changeModel`.
- **What it does**: the **delete-with-confirmation business logic** — `deleteAfterConfirm(thing)`
  branches on kind: a Property in use pops a dire warning + `removePropertyFromAll` (with a
  `ProgressDialog`); a Model with instances warns about orphans; otherwise deletes directly. All
  deletes route through `showSpaceAfterDelete` (which flashes an "undelete" link). Also
  `chooseAModel`/`changeModel` model-picker dialogs (`Future`/callback based), used by the Model
  Designer.
- **Calico notes**: this is **real, reusable business logic** (the safety checks around deletion) —
  preserve it as a service returning `IO`, not bury it in a page. The model-picker dialog is another
  instance of the shared dialog need (§12.1).

---

## 12. Cross-cutting patterns (the important part for planning)

These recur across the packages above and should be designed *once* in the new client:

1. **Confirmation/chooser `Dialog`s are everywhere.** datamodel (delete), history (revert),
   publication (discard), apps (create-space), skilllevel (mode), security (remove members), login.
   All use the `display.Dialog` (jQuery-UI/Bootstrap modal). **A first-class Calico modal/dialog
   component is a shared prerequisite** for almost every feature milestone.

2. **A cluster of features bypasses autowire** and uses raw AJAX through the comm layer
   (`controllers.X.callAjax`): **login** (`clientlogin`), **signup** (`signupStart`), **invites**
   (`handleInvite2`), **password reset** (URL), **photos** (`_photoUpload`, doc 07), and the
   **collaborator autocomplete** (`_getCollaborators`, doc 06). The new client needs a clean
   `IO`-returning wrapper for these too — and several are candidates to move onto proper
   `*Functions` autowire calls (modest, additive server changes per doc 04).

3. **Composable "page-as-a-step" workflows via `Promise`.** `SignUpPage.run`, `TOSPage.run` render
   themselves through `PageManager.renderPage` *without changing the URL* and resolve a `Future`,
   so other flows can `flatMap` them (boot TOS check, invite→signup, login→useApp). In Calico/CE
   these become composable `IO`s naturally — a real ergonomic win over the current Promise plumbing.

4. **Live-update is polling, not push.** notifications (after every page load) and admin
   space-timing (1s `setTimeout`) are the only "live" mechanisms; both carry TODOs wanting
   WebSockets. In the new client these are fs2 `Stream`s; the architecture should leave room for a
   real push channel later.

5. **Cross-cutting client state lives in Ecot singletons** that the new client should model as
   shared `Signal`s in its environment: `UserAccess.user` (current user, synced on every API
   response — doc 01 §7b), `SkillLevel.current` (complexity, invalidated on `setUser`),
   `History.viewingHistory`/version, `Notifications.numNotifications`. These are the client's global
   reactive state.

6. **Progressive disclosure (SkillLevel) gates UI across packages** (editing, security). The new
   client needs an equivalent "current complexity" `Signal` that components consult to show/hide
   advanced affordances.

7. **Server-HTML link-rewriting hacks** recur (notifications `#comment`→`?showComment`; QText URL
   adjustment in doc 01; conversations `fromElem`). Each is a place the server emits a URL the SPA
   can't use directly. The clean fix is structured data from the server (additive) rather than
   client-side string surgery.

8. **jstree appears twice** (apps `ExtractTree`, `display.TreeGadget`) — a shared tree-widget
   replacement decision (doc 03).

9. **Reuse across packages**: publication reuses security's `OnePerm`/`LevelMap` and editing's
   `doSaveChange`; security reuses editing's `propPath`/`getOnePropertyEditor`/`Saveables`/`ItemList`
   (doc 06); datamodel is used by editing/ThingPage/AdvancedPage. The dependency graph is real —
   **editing infra and the security `OnePerm`/`LevelMap` are load-bearing for several "small"
   packages.**

---

## 13. Milestone mapping for these packages

Slotting into doc 04's ordering:

- **Early / read-only-ish (low risk, simple):** search, console, email, notifications (bell +
  page). Good for proving the basic page + API + simple-reactive patterns. notifications also
  establishes the cross-cutting `numNotifications` signal + post-load polling.
- **Needs the dialog component first:** datamodel (delete flows), history (revert), apps
  (create-space), skilllevel (mode chooser). Build the Calico modal early (§12.1).
- **Identity is foundational** (login/signup/TOS/invite) and partly **needed at boot** (TOS check,
  invite landing, the `UserAccess` signal). Schedule the `UserAccess`/`SkillLevel` shared state +
  login/signup forms relatively early, alongside the bootstrap milestone — much else assumes a
  current-user signal exists. Bundle the raw-AJAX wrapper (§12.2) here.
- **After editing:** conversations (server-HTML hooking + `addComment`), publication (reuses
  editing + security `OnePerm`).
- **After security:** publication's permission editing.
- **Apps** after editing/dialogs, gated on the **jstree replacement** (§12.8).
- **Admin** last, and possibly as a *separate* app (doc 04 open question); includes the polling
  pattern (§12.4).
