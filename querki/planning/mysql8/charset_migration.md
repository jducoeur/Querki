# Charset Migration: utf8 (utf8mb3) → utf8mb4

**Status: deferred / not started.** Split out of the MySQL 5.7→8.4 upgrade because it's a separate
concern with its own scope. This doc captures what we know so it isn't lost.

## Why it's deferred, not urgent

`utf8mb3` (what our `DEFAULT CHARSET=utf8` tables actually are) works fine on MySQL 8.4 — it's
*deprecated*, so you get warnings, but nothing breaks. It's therefore **orthogonal to the version
upgrade** and can happen on its own schedule. The pre-upgrade check flagged it as a warning, not an
error (see [`local_mysql_upgrade.md`](local_mysql_upgrade.md)).

The user-visible payoff is the ability to store characters outside the Basic Multilingual Plane —
emoji, some rare CJK — in text fields. If that has never been needed, there's no data-loss pressure.

## Why it's a real project and not "flip four DDLs"

The charset is set in **three** places, and existing data doesn't move on its own:

1. **System DB tables** (`User`, `Identity`, `Spaces`, `SpaceMembership`) — created by
   **`scalajvm/conf/evolutions/default/all.sql`**, the hand-run bootstrap script (see the inventory's
   "System-schema bootstrap" section). This is where the *user-facing* text lives: display names,
   handles, Space names/display. **This is the important one for the emoji payoff** — the per-space
   tables below only hold serialized property blobs.
   - Good news: Play's automatic evolutions are **not** active (no `evolutions` module in `build.sbt`),
     so there's no checksum enforcement — `all.sql` can be edited freely. Only new installs run it.

2. **Per-space / per-user content tables** — created by Querki's own evolution system:
   - `SpaceManagerPersister.scala` (~line 109) — the `s<oid>` thing table
   - `Step4.scala` — `c<oid>` conversations
   - `Step5.scala` — `uv<oid>` user values
   - `UserStep1.scala` — `note<oid>` notifications
   These are safe to change (version-tracked, not checksum-tracked; existing spaces don't re-run them),
   but changing them only affects **newly created** tables. These hold `props`/`propValue` blobs, not
   display strings.

3. **Existing tables** — neither of the above retrofits data already on disk. Converting existing tables
   needs `ALTER TABLE … CONVERT TO CHARACTER SET utf8mb4` per table: a handful of System tables, plus
   potentially many `s<oid>`/`c<oid>`/`uv<oid>`/`note<oid>` tables in the User DB. An RDS engine upgrade
   does **not** do this for you.

## Scope checklist (when this project runs)

- [ ] `all.sql`: change System-table DDL to `utf8mb4` (and consider fixing the historical numbered
      `.sql` files for consistency, though they don't execute).
- [ ] The four app-code DDLs above: `DEFAULT CHARSET=utf8` → `DEFAULT CHARSET=utf8mb4`.
- [ ] **H2 compatibility check:** unit tests (`sbt utst`) may run these `CREATE TABLE`s against H2
      (evolution `doEvolve` is not skipped in in-memory mode). Confirm H2's MySQL mode accepts
      `DEFAULT CHARSET=utf8mb4` before assuming the DDL change is test-safe.
- [ ] Migration script for existing tables (`ALTER TABLE … CONVERT TO CHARACTER SET utf8mb4`), covering
      both System and User DBs, run per environment.
- [ ] Watch index key lengths on conversion. (Current DDLs index only numeric/temporal columns, no
      `VARCHAR`, so today there's no 767-byte-prefix risk — re-verify if any string index is added.)
- [ ] Verify emoji round-trips end to end (display name, comment) after conversion — this is the
      acceptance test that the migration actually delivered the payoff.

## Cross-references

- [`mysql_upgrade_plan.md`](mysql_upgrade_plan.md) — action item 5 points here.
- [`mysql_inventory.md`](mysql_inventory.md) — "System-schema bootstrap" section explains `all.sql`.
