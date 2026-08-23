# AWS RDS MySQL Upgrade (5.7 → 8.0 → 8.4)

The production-facing counterpart to [`local_mysql_upgrade.md`](local_mysql_upgrade.md). Covers the
RDS rollout across the two AWS environments. Read alongside [`mysql_upgrade_plan.md`](mysql_upgrade_plan.md)
(strategy) and [`mysql_smoketests.md`](mysql_smoketests.md) (verification).

## Rollout order

Do the whole thing three times, in this order, and don't start the next until the previous is validated
and has baked:

1. **Local** — the rehearsal (see `local_mysql_upgrade.md`). Already surfaces the real issues.
2. **Test AWS environment** — first RDS run; shake out RDS-specific mechanics on non-critical data.
3. **Production** — only after test is clean.

Within each AWS environment it's still the stepped **5.7 → 8.0**, validate, then **8.0 → 8.4** — RDS
only offers 8.0 as the next major from 5.7, same consecutive-version rule as local.

## What must be true before touching RDS

- **Code is already deployed** carrying the Connector/J 8.0.33 driver and the driver-compat fixes
  (the `LocalDateTime` converter, the `SQLSyntaxErrorException` catch). The driver bump is the risky
  part and should already be proven in that environment against 5.7. See the plan doc.
- **The pre-upgrade check has been run** against that environment's 5.7 data and shows no fatal errors.
  We ran it locally (results in `local_mysql_upgrade.md`) — but **re-run it per environment**, because
  test and prod data differ from your laptop and from each other. Point MySQL Shell at the RDS endpoint:
  ```bash
  mysqlsh -- util check-for-server-upgrade <master>@<rds-endpoint>:3306 --target-version=8.0.46
  ```
  Expect the same three warning classes we saw locally (auth method, `NO_AUTO_CREATE_USER`, `utf8mb3`)
  — none fatal — but confirm nothing new appears on the larger prod dataset.

## RDS-specific mechanics (differ from local)

- **Parameter groups.** A DB parameter group is tied to a major version, so you cannot carry the 5.7
  group forward. Create an **8.0 parameter group** (and later an **8.4** one) mirroring your current
  custom settings, and attach it as part of each upgrade. While building them:
  - Do **not** carry `NO_AUTO_CREATE_USER` in `sql_mode` (removed; blocks startup).
  - `default_authentication_plugin` may be set in the 8.0 group but is **removed in 8.4** — don't put
    it in the 8.4 group.
  - Review `sql_mode` generally; MySQL 8 defaults are stricter. Our queries avoid `ONLY_FULL_GROUP_BY`
    trouble, but the `ListMySpaces` joins are the spot to watch (see smoketests).
- **Snapshots / rollback.** RDS takes an automatic snapshot before a major upgrade, but take a
  **manual snapshot** immediately before each hop too, and confirm it completed. There is **no in-place
  downgrade** — rollback = restore the pre-upgrade snapshot into a new instance. Know that cost before
  you start prod.
- **Downtime.** A major-version upgrade is not zero-downtime in the plain path. For production, strongly
  prefer an **RDS Blue/Green Deployment**: it stands up a green replica on the new version, keeps it in
  sync, lets you test against it, and switches over with minimal downtime and an easy rollback. Use it
  at least for the prod hops; optional for test.
- **Read replicas.** If the environment has read replicas, plan for them — a Blue/Green handles this
  cleanly; the in-place path upgrades replicas after the primary.
- **Backup retention / binlogs.** Ensure automated backups are enabled (required for Blue/Green and for
  snapshot rollback).

## Authentication: convert before 8.4 (same as local, RDS specifics)

The pre-upgrade check flagged that accounts use `mysql_native_password`, which is **disabled by default
in 8.4**. Before the 8.0 → 8.4 hop (while on 8.0), convert the real accounts — the RDS master user and,
critically, **the account the Querki app authenticates as** — to `caching_sha2_password`:

```sql
ALTER USER '<app_user>'@'%' IDENTIFIED WITH caching_sha2_password BY '<pw>';
-- and the master user, per your RDS setup
SELECT user, host, plugin FROM mysql.user;   -- confirm no real account is still on native_password
```

Notes vs. local:
- On RDS the app connects from app hosts, so the grant host is typically `'%'` or a VPC range, **not**
  `@localhost`. Convert whatever host pattern the app account actually uses.
- Leave RDS-internal/`rdsadmin` and system accounts to AWS; only convert accounts you own.
- **TLS makes this easier than local:** RDS supports/encourages TLS, and `caching_sha2_password` over
  TLS needs no `allowPublicKeyRetrieval` flag (the local-only gotcha). Confirm the app's JDBC URL to RDS
  uses SSL; if for some reason it connects without TLS, you'd need `allowPublicKeyRetrieval=true` there
  too — but the right answer for prod is TLS.

## Per-environment procedure

Repeat for **test**, then **production** (using Blue/Green for prod):

1. **Pre-flight:** confirm code deployed; run the pre-upgrade check against this environment; build the
   8.0 parameter group; take a manual snapshot.
2. **Hop 1 — 5.7 → 8.0:** modify the instance (or cut over a Blue/Green green built on 8.0). RDS runs its
   own pre-check, upgrades the datadir and `mysql.*` system tables internally. Reattach the 8.0
   parameter group.
3. **Validate on 8.0:** run the [smoketests](mysql_smoketests.md). Let it bake — this is where a
   surprise should show up on non-critical (test) data first.
4. **Auth conversion:** convert real accounts (app user + master) to `caching_sha2_password` while on
   8.0.
5. **Hop 2 — 8.0 → 8.4:** build the 8.4 parameter group first (no `default_authentication_plugin`),
   manual snapshot, then upgrade / Blue/Green cut over. Reattach the 8.4 group.
6. **Validate on 8.4:** smoketests again; verify the app authenticates (this is where an un-converted
   account would fail).

Don't stack the two hops in one maintenance window.

## Rollback

- Fastest: restore the pre-hop **manual snapshot** into a new instance and repoint the app. (No in-place
  downgrade exists.)
- With **Blue/Green**, rollback before switchover is trivial — just don't cut over. After switchover,
  the old (blue) environment is retained for a window; know your provider's retention before relying on
  it.

## After each environment

- Run the full smoketest checklist; watch the app logs for driver/auth/timezone errors specifically.
- Leave the `utf8mb4` charset migration for its separate project — it is **not** part of this upgrade
  and RDS will not convert existing tables (see the plan doc).
