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
- [ ] **Server-level settings on RDS:** the 8.0 parameter groups deliberately pin
      `character_set_server=utf8` and `collation_server=utf8_bin`, carried over from RDS's 5.7 default
      group, so the upgrade doesn't change them. This project should move them to `utf8mb4` and a
      matching `utf8mb4` collation, and decide whether to keep binary (`_bin`) comparison semantics.
      Also change each schema's default charset (`ALTER DATABASE … CHARACTER SET utf8mb4`), since the
      pre-upgrade check showed `querkisystem`/`querkiuser` default to `utf8`.
- [ ] **H2 compatibility check (confirmed needed):** `scenario.conf`, the environment the mid tests
      (`sbt ftst`) run in, points both `db.system` and `db.user` at in-memory H2
      (`jdbc:h2:mem:…;MODE=MYSQL`). So these `CREATE TABLE`s *do* run through H2 in tests (evolution
      `doEvolve` isn't skipped in in-memory mode). Confirm H2's MySQL mode accepts
      `DEFAULT CHARSET=utf8mb4` before treating the DDL change as test-safe, and check whether the unit
      tests (`sbt utst`) exercise these DDLs too.
- [ ] Migration script for existing tables (`ALTER TABLE … CONVERT TO CHARACTER SET utf8mb4`), covering
      both System and User DBs, run per environment.
- [ ] Watch index key lengths on conversion. (Current DDLs index only numeric/temporal columns, no
      `VARCHAR`, so today there's no 767-byte-prefix risk — re-verify if any string index is added.)
- [ ] Verify emoji round-trips end to end (display name, comment) after conversion — this is the
      acceptance test that the migration actually delivered the payoff.

## Cross-references

- [`mysql_upgrade_plan.md`](mysql_upgrade_plan.md) — action item 5 points here.
- [`mysql_inventory.md`](mysql_inventory.md) — "System-schema bootstrap" section explains `all.sql`.
