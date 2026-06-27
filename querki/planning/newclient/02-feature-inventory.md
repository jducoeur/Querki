# Feature Inventory

Major functionality of the current client, organized by package. For each area: what it does, its
pages/gadgets, and the **server API calls** it makes (the autowire `*Functions` methods — these are
the contract the new client must reproduce). API-call lists below were extracted by grepping
`Client[XFunctions].method` across the client; verify against the actual trait when implementing.

The complete API surface the client uses (call counts in parens), grouped:

- **ThingFunctions**: `getThingInfo`, `getThingPage`, `getProperties`, `getPropertyDisplay`,
  `evaluateQL`, `evaluateQLWithContext`, `getChildren`, `getNumInstances`, `deleteThing`,
  `getAllProperties`, `getAllTypes`, `reloadSpace`.
- **EditFunctions**: `alterProperty`, `create`, `getOnePropertyEditor`, `getPropertyEditors`,
  `addPropertyAndGetEditor`, `removeProperty`, `removePropertyFromAll`, `getPropertyUsage`,
  `changeModel`, `getModelType`.
- **SecurityFunctions**: `getSecurityInfo`, `getAllPerms`, `getOnePerm`, `permsFor`, `getRoles`,
  `getMembers`, `invite`, `removeFromSpace`, `getSharedLinksForRole`, `getOneSharedLink`,
  `getSharedLinkURL`, `archiveThisSpace`.
- **UserFunctions**: `checkTOS`, `agreeToTOS`, `createSpace`, `listSpaces`, `accountInfo`,
  `changePassword`, `changeDisplayName`, `setComplexity`, `validateActivationHash`,
  `resendActivationEmail`.
- **CommonFunctions**: `getStandardThings`, `fetchTOS`, `getProgress`, `acknowledgeComplete`.
- **ConversationFunctions**: `getConversationsFor`, `addComment`, `deleteComment`.
- **PublicationFunctions**: `publish`, `update`, `discardChanges`, `changePublishedModels`.
- **ImportSpaceFunctions**: `importFromXML`, `importFromMySQL`, `getImportProgress`,
  `acknowledgeComplete`.
- **HistoryFunctions**: `getHistorySummary`, `rollbackTo`, `restoreDeletedThing`.
- **AppsFunctions**: `getExtractableModels`, `extractApp`, `addApp`, `checkAppVersions`.
- **AdminFunctions**: `allUsers`, `pendingUsers`, `upgradePendingUser`, `changeUserLevel`,
  `statistics`, `monitor`, `beginSpaceTiming`, `stopSpaceTiming`, `getTimedSpaces`,
  `getSpaceTimingsSince`.
- **NotificationFunctions**: `numNewNotifications`, `getRecentNotifications`, `readThrough`.
- **SearchFunctions**: `search`.
- **EmailFunctions**: `getUnsubOptionsFor`, `unsubscribe`.
- **ConsoleFunctions**: `consoleCommand`.

---

## Core viewing & navigation

### `pages` — the page set
- **ThingPage** (`thingPageFactory`, the catch-all): the central page. Calls
  `ThingFunctions.getThingInfo` (fast, to set title/menu) and `ThingFunctions.getThingPage` (the
  full pre-rendered page incl. headers, stylesheets, the rendered Wikitext body). Hosts a
  **ConversationPane**. Handles `prop` param (render a single Property instead of Default View),
  `ModelLoopException`/`UnknownThingException`. `refresh()` = full reload.
- **ViewPage** (`_view`): minimal Thing view.
- **ExplorePage** (`_explore`): the QL sandbox — type QL, evaluate against a Thing. Uses
  `ThingFunctions.evaluateQL` / `evaluateQLWithContext`.
- **IndexPage** (`_index`): the "you're not in a Space" landing / space list. Uses
  `UserFunctions.listSpaces`.
- **InfoPage** (`_spaceInfo`): Space metadata.
- **AdvancedPage** (`_advanced`): the "Advanced…" commands for a Thing (delete, model info, etc.).
  Uses `ThingFunctions.getNumInstances`, `getChildren`, `deleteThing`, `reloadSpace`.
- **UndeletePage** (`_undelete`): restore deleted Things — `HistoryFunctions.restoreDeletedThing`.
- Base machinery: `Page`, `Pages`/`PagesEcot`, `PageFactoryBase`, `ThingPageFactory`,
  `PageImplicits`, `Exceptions` (`MissingPageParameterException`).

---

## Editing (`editing` — biggest feature, 2037 lines)

The Model Designer and instance editors. The richest, most stateful part of the client.
- **ModelDesignerPage** / **EditInstancesPage** / **CreateAndEditPage** (`_createAndEdit`):
  create/edit Things and Models. Uses `EditFunctions.create`, `getPropertyEditors`,
  `getOnePropertyEditor`, `addPropertyAndGetEditor`, `changeModel`, `getModelType`.
- **EditSpaceInfoPage**: edit Space-level metadata.
- Property management gadgets: **AddPropertyGadget**, **AddExistingPropertyGadget**,
  **CreateNewPropertyGadget**, **PropertyEditor**, **PropValueEditor**, **PropertyDetails**,
  **PropertySection**, **PickLists**, **CheckList**, **DescriptionDiv**, **DeriveNameCheck**,
  **AdvancedEditButton**. These call `EditFunctions.removeProperty`, `removePropertyFromAll`,
  `getPropertyUsage`, plus `ThingFunctions.getProperties`, `getAllProperties`, `getAllTypes`.
- The per-field auto-save mechanism (`alterProperty`) lives in `display.input` (see arch doc §5),
  but is driven by editors created here.

---

## Input controls (`display.input`)

The hookable edit widgets (registered selectors → gadgets). Each auto-saves via
`EditFunctions.alterProperty`. Members: **TextInputGadget** (+ large), **TagSetInput** /
MarcoPolo **`_tagInput`** (autocomplete tags, uses the manifest/MarcoPolo facade), **SelectGadget**,
**CheckboxGadget**, **OptYesNoGadget**, **RatingGadget** (raty facade), **DateGadget** (datepicker
facade), **SortableList** (jQuery UI sortable), **DeleteButton** (`_deleteInstanceButton`),
**DependentInputGadget**/`ForProp`/`InputDependencies` (fields whose options depend on other
fields), **NoAutoSave** marker. Plus the `InputGadget`/`InputGadgets` framework.

---

## Conversations (`conversations`)

Comment threads attached to a Thing. **ConversationPane** (loaded inside ThingPage),
**ConversationGadget**, **CommentGadget**, **ReplyGadget**, **NewConversationGadget**. API:
`ConversationFunctions.getConversationsFor`, `addComment`, `deleteComment`.

---

## Security / sharing (`security` — 1832 lines)

- **SecurityPage** (`_security`): per-Thing permissions. `SecurityFunctions.getSecurityInfo`,
  `getAllPerms`, `getOnePerm`, `permsFor`. Gadgets: **PermCheckboxes**, **OnePerm**, **ItemList**,
  **Saveables**.
- **SharingPage** (`_sharing`): Space membership & invitations. `getRoles`, `getMembers`, `invite`,
  `removeFromSpace`, shared-link management (`getSharedLinksForRole`, `getOneSharedLink`,
  `getSharedLinkURL`). Gadgets: **MembersTab**, **InvitationTab**, **CustomRolesTab**,
  **PersonDisplay**, **RoleInfo**, **RolesDisplay**, **StandardRoleDisplay**, **CustomRoleDisplay**,
  **EditRolePanel**, **EditShareableInvite**.
- **archiveThisSpace** (delete/archive a whole Space) lives here too.

---

## Identity / accounts (`identity`)

- **UserManager** / `UserAccess`: current-user state, login status, kept in sync via `setUser` on
  every API response.
- **SignUpPage**, **ValidateSignupPage** (`UserFunctions.validateActivationHash`,
  `resendActivationEmail`), **HandleInvitePage** (accept a Space invitation), **TOSPage** (Terms of
  Service: `CommonFunctions.fetchTOS`, `UserFunctions.checkTOS`/`agreeToTOS`).
- **AccountPage** (`_account`): `UserFunctions.accountInfo`, `changePassword`, `changeDisplayName`.
- **SkillLevel** (`identity.skilllevel`): progressive disclosure — hides advanced UI from novices.
  `UserFunctions.setComplexity`. `SkillLevelsNeeded` annotates UI elements with required level.

---

## Space lifecycle (`pages`, `apps`, `datamodel`)

- **CreateSpacePage** (`_createSpace`): `UserFunctions.createSpace`.
- **ImportSpacePage** (`_importSpace`): import a Space from XML or MySQL —
  `ImportSpaceFunctions.importFromXML`/`importFromMySQL`, with progress polling
  (`getImportProgress`, `acknowledgeComplete`) via the **ProgressDialog**.
- **apps**: the "Apps" feature (a Space can be based on / extract into a reusable App).
  `AppsFunctions.getExtractableModels`, `extractApp`, `addApp`, `checkAppVersions`.
- **datamodel** (`DataModelEcot`): client-side helpers for the data model (Kinds, Things) and
  delete/create flows — `ThingFunctions.deleteThing`, `EditFunctions.create`.

---

## Publication (`publication`)

Draft/publish workflow for Spaces that use it. `PublicationFunctions.publish`, `update`,
`discardChanges`, `changePublishedModels`.

---

## History (`history`)

"View this Space as of a past time" + undo. **HistoryEcot**/`History` (the cross-cutting mode
service — see arch doc §8), plus the history-summary page. `HistoryFunctions.getHistorySummary`,
`rollbackTo`, `restoreDeletedThing`. When in history mode, API calls are filtered
(`isLegalDuringHistory`) and carry a version param.

---

## Search (`search`)

`SearchFunctions.search`; renders results (with the search box typically in the menu bar).

---

## Notifications (`notifications`)

The notification bell/feed. `NotificationFunctions.numNewNotifications`, `getRecentNotifications`,
`readThrough`.

---

## Admin (`admin`) — site operators only

- **ManageUsersPage**: `AdminFunctions.allUsers`, `pendingUsers`, `upgradePendingUser`,
  `changeUserLevel`.
- **StatisticsPage**: `AdminFunctions.statistics`.
- **MonitorPage**: `AdminFunctions.monitor`.
- **SpaceTimingPage**: perf timing — `beginSpaceTiming`, `stopSpaceTiming`, `getTimedSpaces`,
  `getSpaceTimingsSince`.

---

## Console (`console`)

In-Space command console. `ConsoleFunctions.consoleCommand`.

---

## Email (`email`)

**UnsubscribePage** (`_doUnsub`, often the server forces navigation here on load):
`EmailFunctions.getUnsubOptionsFor`, `unsubscribe`.

---

## Photos (`photos`)

Image upload & display. Built on a chain of jQuery FileUpload plugins + load-image/canvas-to-blob
(client-side resize) — these go through the **low-level `comm` layer** (multipart upload to a Play
route), not autowire. **This is the area most coupled to the dead jQuery FileUpload stack** and
will need the most rethinking (doc 03/04).

---

## Reusable display gadgets (`display`)

Not features per se, but the shared UI vocabulary the pages are built from. Many will become Calico
components:
- Chrome: **MenuBar**, **MenuButton**, **TabGadget**, **StandardFooter**, **StatusLine**.
- Buttons: **ButtonGadget**, **SmallButtonGadget**, **LinkButton**, **QLButtonGadget** (a button
  that runs QL — `_qlInvoke`), **NavigateGadget** (`_navigateImmediately`).
- Containers/overlays: **Dialog** (jQuery UI dialog facade), **ProgressDialog**, **WithTooltip**
  (`_withTooltip`), **WrapperDiv**.
- Content: **QText**/**QTextSpan**/**RawDiv**/**RawSpan** (server-HTML hosting + link rewriting),
  **TreeGadget**/**QLTree** (`_tree`/`_qlTree`, jstree facade), **HistogramGadget** (`.histogram`),
  **PagePaginator**.
- Reactive form helpers (`display.rx`): **RxText**, **RxSelect**, **RxCheckbox**, **RxRadio**,
  **RxButtonGroup**, **RunButton**, **QuerkiEmptyable**.
- Utilities: **QuerkiUIUtils** (Scalatags helpers: `thingUrl`, `wikitext`, etc.),
  **GadgetListeners** (intra-page gadget pub/sub).

---

## Localization (`local`) & QText utils (`qtext`)

- **Localization**/`LocalizationEcot`: message bundles keyed by package/page; `Localization.ready`
  gates rendering.
- **QTextUtils**: `adjustUrl` (server URL → client hash), wikitext helpers. (The actual Wikitext
  *parser* lives in `querki.qtext` in **shared** code, used by both client and server.)
