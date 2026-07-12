# Local MySQL: Where It Lives & How to Upgrade It (5.7 → 8.0 → 8.4)

Reference for upgrading the **local development** MySQL, written to mirror the AWS RDS upgrade path so
the local run doubles as a rehearsal. Companion to [`mysql_upgrade_plan.md`](mysql_upgrade_plan.md)
and [`mysql_smoketests.md`](mysql_smoketests.md).

## Where the local instance actually is

Despite `local.conf` pointing the app at `jdbc:mysql://host.docker.internal/`, the database is **not**
in Docker. `host.docker.internal` resolves, from inside the app's Docker container, to the host Mac —
so the containerized server reaches *out* to a MySQL running natively on the host. Only the app is
containerized; the DB never was. (The old MySQL 5.7 image sitting in Docker Desktop is unused leftover
cruft — safe to delete, and worth deleting to stop the confusion.)

The real instance, as of this writing:

- **Install:** Homebrew formula `mysql@5.7`, Apple Silicon (`/opt/homebrew`).
- **How it runs:** `brew services` registered a per-user LaunchAgent
  (`~/Library/LaunchAgents/homebrew.mxcl.mysql@5.7.plist`), so macOS auto-starts it at login. That's
  why it's "just been running for years" with no obvious how.
- **Process:** `/opt/homebrew/opt/mysql@5.7/bin/mysqld --datadir=/opt/homebrew/var/mysql`, via
  `mysqld_safe`, listening on `127.0.0.1:3306`.
- **Data directory:** `/opt/homebrew/var/mysql` (~210 MB — small; dumps/restores are seconds).

Quick re-discovery commands if this drifts:
```bash
lsof -nP -iTCP:3306 -sTCP:LISTEN     # what's listening on 3306
ps aux | grep "[m]ysqld"             # the actual mysqld invocation + datadir
brew services list                   # confirms the LaunchAgent + status
```

## Why the upgrade must be stepped

MySQL only supports in-place datadir upgrades between **consecutive** major versions. A direct
5.7 → 8.4 jump is unsupported — which is exactly why **AWS RDS only offers 8.0** as the next hop, and
why local should follow the same 5.7 → 8.0 → 8.4 sequence. Each hop is an independent, separately
validated upgrade.

Homebrew has both intermediate formulae available: `mysql@8.0` (8.0.46) and `mysql@8.4` (8.4.10, the
LTS target). Do **not** use the default `mysql` formula — it's now 9.x, past our target.

## Step 0 (do this first, gates everything): pre-upgrade check

RDS runs an equivalent check and will **block** the 5.7 → 8.0 upgrade on incompatibilities. Run the
official checker locally against 5.7 first, so any blockers surface on your laptop, not in production:

```bash
brew install mysql-shell
mysqlsh -- util check-for-server-upgrade root@localhost:3306 --target-version=8.0.46
```

It reports removed features, obsolete data types, orphaned tables, and reserved-word identifiers.
Querki-specific things to expect:

- **`utf8mb3` usage** — our `DEFAULT CHARSET=utf8` tables. Flagged as a *warning*, not a blocker. (The
  charset migration to `utf8mb4` is a separate project — see the plan doc.)
- **Reserved-word identifiers** — 8.0 added a batch of reserved words. Notably `SYSTEM` became
  reserved, and the System DB is literally named `system`; let the checker say authoritatively whether
  that (or any column name) is an actual problem in the current schema.

Fix anything rated an **error** before upgrading either environment. Re-run with
`--target-version=8.4.10` before the second hop, too.

## Local upgrade — stepped in-place (the AWS rehearsal)

The Homebrew shared-datadir quirk works in our favor here: every formula defaults to
`/opt/homebrew/var/mysql`, so each newly-installed version boots against the prior version's datadir
and upgrades it in place — the same datadir-format upgrade RDS performs internally. Since 8.0.16 the
server auto-upgrades on first startup (no separate `mysql_upgrade` step).

```bash
# Safety net first (trivial at ~210 MB). Dump only the app DBs, NOT the mysql/sys/perf system schemas:
mysqldump -u root --databases system user template test_system_template \
  --routines --triggers --single-transaction > ~/querki-mysql57-backup.sql

# --- Hop 1: 5.7 -> 8.0 ---
brew services stop mysql@5.7
brew unlink mysql@5.7
brew install mysql@8.0
brew link --force mysql@8.0
brew services start mysql@8.0     # boots against the 5.7 datadir and upgrades it
#   verify: mysql --version; log in; run the smoketests

# --- Hop 2: 8.0 -> 8.4 ---
brew services stop mysql@8.0
brew unlink mysql@8.0
brew install mysql@8.4
brew link --force mysql@8.4
brew services start mysql@8.4     # upgrades the 8.0 datadir to 8.4
#   verify again

# --- Cleanup once confident ---
brew uninstall mysql@5.7 mysql@8.0
# and delete the stale Docker 5.7 image
```

If a hop refuses to start, read the datadir error log — the upgrade complaints land there and will
echo whatever the checker warned about:
```bash
cat /opt/homebrew/var/mysql/*.err
```

### Alternative: if you only want a working 8.4 dev env (not a rehearsal)

A logical dump + restore straight into a fresh 8.4 skips the datadir dance entirely (you replay SQL
rather than upgrading binary files, so the consecutive-version rule doesn't apply). Faster and simpler
for a pure "get local onto 8.4" goal — but it does **not** exercise the datadir upgrade AWS will do, so
it's a weaker rehearsal. Procedure: dump (as above) → `brew services stop/unlink mysql@5.7` →
`mv /opt/homebrew/var/mysql /opt/homebrew/var/mysql-5.7-old` → `brew install mysql@8.4` +
`brew services start mysql@8.4` (inits a fresh datadir) → `mysql -u root < backup.sql` → recreate the
app's DB user/grants (the `mysql` system schema wasn't restored, so the app's login must be
re-created; the 8.0.33 driver handles 8.x's default `caching_sha2_password` fine).

## AWS RDS path (for reference — the real target)

1. Take a **manual snapshot** (RDS also auto-snapshots, but be deliberate).
2. Modify instance → engine **8.0.x**. RDS runs its pre-check, then upgrades the datadir and `mysql.*`
   system tables internally. Expect **meaningful downtime** for a major-version upgrade.
3. **Validate on 8.0** — run the smoketests; let it bake.
4. Later, a **separate** upgrade 8.0 → **8.4**, same shape. Don't stack both in one maintenance window.

The pre-upgrade checker (Step 0) is what tells you, ahead of time, whether RDS's 5.7 → 8.0 pre-check
will pass.
