# Querki MySQL Upgrade — Smoketest Outline

A modest, hand-runnable checklist to gain reasonable confidence that the MySQL-backed features still
work after the driver/code upgrade and again after the server upgrade. Grounded in what the tables are
actually used for — see [`mysql_inventory.md`](mysql_inventory.md) for the query-level detail and
[`mysql_upgrade_plan.md`](mysql_upgrade_plan.md) for the strategy.

This is deliberately *not* exhaustive. It targets the read/write paths most likely to be disturbed by
a driver or server change, one concrete user action per query cluster.

## How to use this

Run the whole list at **two checkpoints**:

1. **After the code upgrade, still on MySQL 5.7.** This isolates driver/code regressions from
   server regressions. Anything that breaks here is a code problem, not a server problem.
2. **After the AWS DB upgrade to MySQL 8.x.** Re-run the same list. Anything that newly breaks here
   is server/sql-mode/charset related.

Most items are plain end-user actions in the running app. A few (admin, old-style Space load) need
specific setup and are marked. Work top-down — later areas reuse the account and Space from earlier
ones.

## Coverage map (feature → tables → representative queries)

| Feature area | Tables | Key queries exercised |
|---|---|---|
| Account & login | `User`, `Identity` (System) | `loadByHandle`/`loadByEmail`, `createUser`, `changePassword`, `changeDisplayName`, `setTOSVersion` |
| Space lifecycle | `Spaces` (System), `s<oid>` + evolutions (User) | `doCreateSpace` (incl. CREATE TABLE + evolution chain), `ListMySpaces`, `GetSpaceByName`, Space rename |
| Sharing / membership | `SpaceMembership` (System) | `addSpaceMembership`, `deleteSpaceMembership`, `ListMySpaces` member-of join, `getAcquaintanceIds` |
| Old-style Space content | `s<oid>` (User) | `SpacePersister.Load` (`information_schema` check + `SELECT * FROM s<oid>`) |
| Conversations | `c<oid>` (User) | `AddComment`, `LoadCommentsFor`, `UpdateComment` |
| User values | `uv<oid>` (User) | `SaveUserValue` (insert/update/delete), `LoadValuesForUser`, `LoadAllPropValues` |
| Notifications | `User.lastNoteChecked` (System), `note<oid>` (User) | `createNotification`, `loadCurrent`, `loadUserInfo`, `updateLastChecked` |

## Smoketests

### 1. Account & login
- [ ] **Sign up a brand-new account.** Exercises `createUser` (INSERT `User` + `Identity`, `join_date`
      timestamp write) and `findOrCreateIdentityByEmail`. Watch: successful create, no timestamp error.
- [ ] **Log in by handle, then log out and log in by email.** Exercises `loadByHandle` and
      `loadByEmail` → `userParser` (oid/str/int reads + password auth).
- [ ] **Change your password**, then re-login with the new one. Exercises `changePassword` (UPDATE
      `Identity`) and re-validates the auth round-trip.
- [ ] **Change your display name.** Exercises `changeDisplayName` (UPDATE `Identity`) + cache
      invalidation; confirm the new name shows after reload.
- [ ] **(If prompted) accept the Terms of Service.** Exercises `setTOSVersion` (UPDATE `User`).
- [ ] **(Admin only) change another user's level.** Exercises the `requireAdmin` path + `changeUserLevel`
      (UPDATE `User`).

### 2. Space lifecycle
- [ ] **Create a new Space.** The single most valuable test: name-collision check, quota check, `CREATE
      TABLE s<oid>` DDL, INSERT into `Spaces`, and the **evolution chain** (Step2→6 creating `c<oid>`,
      `uv<oid>`, owner membership). Confirm the Space opens afterward.
- [ ] **Try to create a second Space with the same name.** Should be rejected — confirms the
      name-collision `COUNT(*)` query still returns correctly.
- [ ] **View your dashboard / "my Spaces" list.** Exercises `ListMySpaces` (owned join + member-of
      join). The multi-JOIN queries here are the most sql-mode-sensitive ones — worth close attention
      on the **post-DB-upgrade** run.
- [ ] **Rename a Space.** Exercises the Space-rename UPDATE against `Spaces`; confirm both the name and
      display name update.
- [ ] **Navigate to a Space by its URL slug.** Exercises `GetSpaceByName` → `oid` resolution.

### 3. Old-style Space content *(only if a pre-Akka-Persistence Space is available)*
- [ ] **Load a legacy Space that still has a MySQL `s<oid>` thing table.** This is the *only* path that
      exercises `SpacePersister.Load` — the `information_schema` existence check plus `SELECT * FROM
      s<oid>` via `rawSpaceParser`. New spaces store Things in Cassandra and will **not** cover this.
      If prod has such spaces, load at least one and confirm its Things render.

### 4. Conversations
- [ ] **Post a comment on a Thing.** Exercises `AddComment` (INSERT `c<oid>`, incl. `createTime`).
- [ ] **Reload the Thing and view the comment.** Exercises `LoadCommentsFor` → `commentParser`
      (dateTime + the five `bool` columns + oid). *(Already verified once during the driver work —
      re-run at both checkpoints.)*
- [ ] **Edit the comment, then delete it.** Exercises `UpdateComment` (the `isEdited` / `isDeleted`
      boolean writes and re-reads).

### 5. User values *(pick a Space with a Rating, Like, or other user-value property)*
- [ ] **Set a user value** (e.g. rate a Thing). Exercises `SaveUserValue` INSERT into `uv<oid>`.
- [ ] **Change that value**, then **clear it.** Exercises the UPDATE and DELETE branches.
- [ ] **View the value back / an aggregate.** Exercises `LoadValuesForUser` / `LoadAllPropValues` →
      `rawUVParser` (oid×3 + `modTime` dateTime). *(This was the original failure site — good to
      re-confirm.)*

### 6. Notifications
- [ ] **Trigger a notification** (e.g. have a second account comment on, or accept a share to, your
      Thing/Space). Exercises `createNotification` INSERT into `note<oid>`.
- [ ] **Check the unread-count badge**, then **open the notifications list.** Exercises `loadUserInfo`
      (`lastNoteChecked`), `loadCurrent` → `notificationParser` (dateTime + bool), and
      `updateLastChecked`.
- [ ] **Brand-new account checks notifications immediately after signup.** Deliberately hits the race
      where the `note<oid>` table may not exist yet — this is the path guarded by the
      **rewritten exception catch** (`java.sql.SQLSyntaxErrorException` replacing the old Connector/J
      class). Confirm it degrades gracefully (empty list, no crash) rather than throwing.

### 7. Sharing / membership
- [ ] **Share a Space with a second account / send an invite.** Exercises `addSpaceMembership` INSERT.
- [ ] **As the invited account, confirm the Space appears in its "member-of" list.** Exercises the
      `ListMySpaces` member-of join from the other side.
- [ ] **Remove that member.** Exercises `deleteSpaceMembership` DELETE.
- [ ] **(Optional) check sharing autocomplete / acquaintance suggestions.** Exercises
      `getAcquaintanceIds` (the self-join on `SpaceMembership`).

## Cross-cutting watch items

These aren't separate tests so much as things to keep an eye on while doing the above:

- **Timestamps read back sane, not shifted.** While viewing comments, notifications, and "modified"
  times, sanity-check that displayed times are correct and not off by a fixed number of hours. This is
  the `LocalDateTime` / `ZoneId.systemDefault` fix in the flesh — a timezone regression would show up
  as a consistent offset.
- **Non-BMP characters (emoji) in text.** Try putting an emoji in a comment or display name. Existing
  tables are `utf8mb3` (`DEFAULT CHARSET=utf8`), so this is expected to fail/mangle *until* the
  separate charset-migration project runs — verify the behavior matches expectation rather than
  assuming, and note it's a known limitation, not a new regression.
- **sql-mode sensitivity (post-DB-upgrade only).** MySQL 8 defaults differ (e.g. stricter
  `ONLY_FULL_GROUP_BY`). Our queries avoid `GROUP BY`, but the multi-JOIN `ListMySpaces` queries are
  where any surprise would surface — give area 2's dashboard test extra attention on the second run.

## Explicitly out of scope

- **Cassandra / Akka Persistence** — Thing storage for modern Spaces is not MySQL and isn't covered
  here.
- **Attachments / BLOB path** — `SpacePersister.byteArrayToStatement` exists for attachment blobs; if
  that path is still live in your deployment, add an upload/view test, since BLOB handling is
  driver-sensitive. It's omitted here because it appears dormant in the current query set.
- **Automated suites** — `sbt utst` / `sbt ftst` and the functional (browser) tests remain the real
  regression net; this outline is only a fast human confidence check on top of them.
