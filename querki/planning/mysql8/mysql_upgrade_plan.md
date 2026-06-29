# Querki MySQL 5.7 → 8.x Upgrade Plan

The companion catalog of every Anorm query is in [`mysql_inventory.md`](mysql_inventory.md). This
document holds the upgrade *strategy*: sequencing, action items, and the gotchas we've hit. All paths
are relative to `querki/`.

## Environments

- **Production and test** run on AWS (RDS). The server already bundles Connector/J **8.0.33**
  (see `build.sbt`).
- **Local development** does *not* run on AWS, and is where driver/DB issues surface first.

## Upgrade sequencing: code-first is viable (no big-bang required)

All of the code changes below are backward-compatible with MySQL 5.7, so we can ship the code first
and upgrade the AWS databases afterward. There is **no** need to flip both at once.

Key reason: the driver is already on Connector/J 8.x, so we don't face the classic
`caching_sha2_password` authentication break (Connector/J 5.x can't talk to a default-auth MySQL 8
server). That landmine is already behind us.

Recommended order:
1. Deploy the code fixes (all safe against 5.7).
2. Upgrade the AWS DB 5.7 → 8.x.

The one fix that most wants to be deployed *before* the DB upgrade is the `information_schema`
`table_schema = DATABASE()` filter (item 3 below) — it's a hygiene measure against ambiguous
schema visibility.

### The risk lives in the driver bump, not the server version

The most dangerous part of this whole effort is the Connector/J version jump (5.1.x → 8.0.33),
**not** the 5.7 → 8.x server upgrade. The driver changes value-mapping behavior in ways that bite at
read time regardless of which server version you point at. See the DateTime gotcha below — it would
have failed identically against a 5.7 server. Lesson: exercise real read/write across every persisted
column type (especially temporal, `BIT`/`BOOLEAN`, `BIGINT UNSIGNED`, `BLOB`) before trusting the
driver upgrade. Unit/mid tests against real MySQL catch this; a quick "create a Space" smoke test does
not, because writes succeed and only reloads fail.

## Does upgrading the AWS DB retrofit existing tables?

**No.** An RDS 5.7 → 8.0 upgrade runs the internal catalog upgrade but does **not** `ALTER` any user
tables. Specifically:
- The server's *default* charset becomes `utf8mb4`, but that only affects **newly created** tables
  that don't specify a charset. Existing `DEFAULT CHARSET=utf8` (= `utf8mb3`) tables stay `utf8mb3`.
- `utf8` remains a valid (deprecated) alias for `utf8mb3` in 8.0 and 8.4 — existing tables and the
  DDL that creates new ones keep working, just with deprecation warnings.
- Converting existing data is a separate migration: `ALTER TABLE ... CONVERT TO CHARACTER SET
  utf8mb4` per table. For Querki that's a handful of System-DB tables plus potentially millions of
  per-Space (`s<oid>`, `c<oid>`, `uv<oid>`) and per-user (`note<oid>`) tables. Treat as its own
  project, not part of the server upgrade.

The only thing `utf8mb3` can't store that `utf8mb4` can is characters outside the BMP (emoji, some
rare CJK). If Querki's text fields have never needed those, there's no immediate data-loss risk.

## Gotchas hit so far

### Connector/J 8.x returns `DATETIME` as `java.time.LocalDateTime` (FIXED)

**Symptom:** A brand-new Space writes fine, then fails to *reload* with:
```
anorm.AnormException: TypeDoesNotMatch(Cannot convert column ColumnName(uv<oid>.modTime,Some(modTime)),
value 2026-06-27T21:58:19 to date:class java.time.LocalDateTime)
```

**Root cause:** Connector/J 8.x's `ResultSet.getObject()` returns `java.time.LocalDateTime` for
`DATETIME` columns, where the 5.1.x driver returned `java.sql.Timestamp`. The custom Anorm converter
in `scalajvm/app/querki/time/TimeAnorm.scala` only had a `case` for `java.sql.Timestamp`, so the new
value type fell through to `TypeDoesNotMatch`. Writes were unaffected because the write path
(`setTimestamp`) is unchanged — hence the write-ok / read-fail asymmetry.

**Important nuance:** the schema has *both* temporal types. `Step2` adds `modified TIMESTAMP` to the
thing table, while `modTime`/`createTime`/`sentTime` are `DATETIME`. Connector/J 8.x maps `TIMESTAMP`
→ `java.sql.Timestamp` but `DATETIME` → `LocalDateTime`. So the converter must keep **both** cases —
dropping the `Timestamp` case would fix the user-values/conversation/notification reads but break
thing-table reads.

**Fix applied:** added a `java.time.LocalDateTime` case alongside the existing `Timestamp` case,
interpreting the `LocalDateTime` in `ZoneId.systemDefault` (which reproduces the old
`Timestamp.getTime` read semantics, so round-trip behavior is unchanged from before the driver bump).

### Parser verification status (under Connector/J 8.0.33)

The custom/shared row parsers have been exercised against the new driver by loading real data locally:

- **`dateTime`** (`TimeAnorm.scala`) — VERIFIED. Reads `DATETIME` (now `LocalDateTime`) cleanly after
  the fix above. Originally the failing case.
- **`oid`** (`SqlHelpers.scala`, reads `BIGINT` as `Long`) — VERIFIED by loading a small real Space
  (System-DB reads: `Spaces.owner`, `GetSpaceByName`, user/identity lookup). `BIGINT` → `Long`
  mapping is unchanged across driver versions, so this was low-risk; now empirically confirmed.
- **`bool`** (`SqlParser.bool`, reads `BOOLEAN`/`TINYINT(1)`) — VERIFIED by posting a comment and
  reloading it (`commentParser` reads `primaryResponse`, `needsModeration`, `isEdited`, `isDeleted`,
  `isArchived`). Confirms the driver's `tinyInt1isBit=true` default still yields real Booleans.

Remaining lower-priority parsers (`int`, `str`, nullable variants) are standard Anorm and ride along
with the reads above. No outstanding parser concerns under the new driver.

## Action items

### Definite

1. **Old Connector/J exception class** (`NotificationPersistence.scala`, ~line 107): catches
   `com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException`, which doesn't exist in Connector/J
   8.x. Catch the JDBC-standard `java.sql.SQLSyntaxErrorException` instead. *(Being changed as part of
   the current driver work.)*

2. **`DEFAULT CHARSET=utf8` → `utf8mb4`** in the four CREATE TABLE DDLs (`Step4`, `Step5`,
   `UserStep1`, and the thing-table creation in `SpaceManagerPersister` ~line 103). Note: changing the
   DDL only affects **newly created** tables; it does not retrofit existing ones (see charset section
   above).

3. **`information_schema` filter** (`SpacePersister.scala` ~line 183): add
   `AND table_schema = DATABASE()` to the table-existence check. Without it, ambiguous schema
   visibility could miscount and mis-classify a Space's old-style-table status. Deploy this one
   *before* the DB upgrade.

4. **`INSERT Identity (...)` without `INTO`** (`UserPersistence.scala` ~line 411): non-standard MySQL
   extension; still works in 8.x but worth normalizing to `INSERT INTO`.

5. **`bigInt` casing** (`Step4.scala` ~line 18): normalize `bigInt` → `bigint`. Cosmetic; MySQL type
   names are case-insensitive.

### Likely non-issues, but verify

6. **`BOOLEAN`/`TINYINT`**: stored as `TINYINT(1)`, read as Int via `SqlHelpers.bool()`. Unchanged in
   MySQL 8 — but see the driver-bump follow-up above.

7. **`MEDIUMTEXT` (16 MB) vs `max_allowed_packet`**: MySQL 8 default is 64 MB (up from 4 MB in 5.7),
   so less of a concern — but confirm the production RDS parameter group setting.

8. **Anorm version**: Anorm 2.x (Play 2.8) is JDBC-based and not MySQL-version-sensitive. No Anorm
   changes expected. *(The driver, not Anorm, is the moving part — see gotchas.)*

9. **`ONLY_FULL_GROUP_BY`**: MySQL 8 enforces this by default. The queries avoid the trap — the one
   aggregation-ish query (`getAcquaintanceIds`) uses `DISTINCT`, not `GROUP BY`. No change needed.

10. **Cross-DB transactions**: operations spanning System + User DBs already can't be a single
    transaction; unchanged in MySQL 8.

11. **Dynamic table names**: `{tname}`/`{cname}`/`{uvname}`/`{notename}` are interpolated as raw
    strings (parameterized table names don't work in MySQL). Derived from OIDs, not user input — no
    injection risk, no MySQL 8 change needed.
