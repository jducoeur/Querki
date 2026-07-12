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

### Results of the Step 0 Upgrade Check

No hard errors, but there were some warnings, so recording here the full output from the mysqlsh run:

```
The MySQL server at localhost:3306, version 5.7.44 - Homebrew, will now be
checked for compatibility issues for upgrade to MySQL 8.0.46.

1) Check for orphaned routines and events in 5.7 (orphanedObjects)
   No issues found

2) Usage of old temporal type (oldTemporal)
   No issues found

3) Usage of db objects with names conflicting with new reserved keywords
(reservedKeywords)
   No issues found

4) Usage of utf8mb3 charset (utf8mb3)
   Warning: The following objects use the deprecated utf8mb3 character set. It
   is recommended to convert them to use utf8mb4 instead, for improved Unicode
   support. The utf8mb3 character is subject to removal in the future.

   querkisystem - schema's default character set: utf8
   querkiuser - schema's default character set: utf8
   querkisystem.Identity.authentication - column's default character set: utf8
   querkisystem.Identity.email - column's default character set: utf8
   querkisystem.Identity.handle - column's default character set: utf8
   querkisystem.Identity.name - column's default character set: utf8
   querkisystem.SpaceMembership.nickname - column's default character set: utf8
   querkisystem.Spaces.display - column's default character set: utf8
   querkisystem.Spaces.name - column's default character set: utf8
   querkisystem.User.name - column's default character set: utf8
   querkiuser.c7w4g7wg.props - column's default character set: utf8
   querkiuser.c7w4g7wh.props - column's default character set: utf8
   querkiuser.c7w4g7wj.props - column's default character set: utf8
   querkiuser.c7w4g7wk.props - column's default character set: utf8
   querkiuser.c7w4g7wv.props - column's default character set: utf8
   querkiuser.c7w4jd36.props - column's default character set: utf8
   querkiuser.c7w4jd4p.props - column's default character set: utf8
   querkiuser.noteb.props - column's default character set: utf8
   querkiuser.notev.props - column's default character set: utf8
   querkiuser.s2avih.props - column's default character set: utf8
   querkiuser.s2avih_Backup1.props - column's default character set: utf8
   querkiuser.s7w4g7wg.props - column's default character set: utf8
   querkiuser.s7w4g7wg_Backup1.props - column's default character set: utf8
   querkiuser.s7w4g7wg_Backup2.props - column's default character set: utf8
   querkiuser.s7w4g7wg_Backup3.props - column's default character set: utf8
   querkiuser.s7w4g7wg_Backup4.props - column's default character set: utf8
   querkiuser.s7w4g7wg_Backup5.props - column's default character set: utf8
   querkiuser.s7w4g7wh.props - column's default character set: utf8
   querkiuser.s7w4g7wh_Backup1.props - column's default character set: utf8
   querkiuser.s7w4g7wh_Backup2.props - column's default character set: utf8
   querkiuser.s7w4g7wh_Backup3.props - column's default character set: utf8
   querkiuser.s7w4g7wh_Backup4.props - column's default character set: utf8
   querkiuser.s7w4g7wh_Backup5.props - column's default character set: utf8
   querkiuser.s7w4g7wj.props - column's default character set: utf8
   querkiuser.s7w4g7wj_Backup1.props - column's default character set: utf8
   querkiuser.s7w4g7wj_Backup2.props - column's default character set: utf8
   querkiuser.s7w4g7wj_Backup3.props - column's default character set: utf8
   querkiuser.s7w4g7wj_Backup4.props - column's default character set: utf8
   querkiuser.s7w4g7wj_Backup5.props - column's default character set: utf8
   querkiuser.s7w4g7wk.props - column's default character set: utf8
   querkiuser.s7w4g7wk_Backup1.props - column's default character set: utf8
   querkiuser.s7w4g7wk_Backup2.props - column's default character set: utf8
   querkiuser.s7w4g7wk_Backup3.props - column's default character set: utf8
   querkiuser.s7w4g7wk_Backup4.props - column's default character set: utf8
   querkiuser.s7w4g7wk_Backup5.props - column's default character set: utf8
   querkiuser.s7w4g7wv.props - column's default character set: utf8
   querkiuser.s7w4g7wv_Backup1.props - column's default character set: utf8
   querkiuser.s7w4g7wv_Backup2.props - column's default character set: utf8
   querkiuser.s7w4g7wv_Backup3.props - column's default character set: utf8
   querkiuser.s7w4g7wv_Backup4.props - column's default character set: utf8
   querkiuser.s7w4g7wv_Backup5.props - column's default character set: utf8
   querkiuser.s7w4jd36.props - column's default character set: utf8
   querkiuser.s7w4jd36_Backup1.props - column's default character set: utf8
   querkiuser.s7w4jd36_Backup2.props - column's default character set: utf8
   querkiuser.s7w4jd36_Backup3.props - column's default character set: utf8
   querkiuser.s7w4jd36_Backup4.props - column's default character set: utf8
   querkiuser.s7w4jd36_Backup5.props - column's default character set: utf8
   querkiuser.s7w4jd4p.props - column's default character set: utf8
   querkiuser.s7w4jd4p_Backup1.props - column's default character set: utf8
   querkiuser.s7w4jd4p_Backup2.props - column's default character set: utf8
   querkiuser.s7w4jd4p_Backup3.props - column's default character set: utf8
   querkiuser.s7w4jd4p_Backup4.props - column's default character set: utf8
   querkiuser.s7w4jd4p_Backup5.props - column's default character set: utf8
   querkiuser.uv7w4g7wg.propValue - column's default character set: utf8
   querkiuser.uv7w4g7wh.propValue - column's default character set: utf8
   querkiuser.uv7w4g7wj.propValue - column's default character set: utf8
   querkiuser.uv7w4g7wk.propValue - column's default character set: utf8
   querkiuser.uv7w4g7wv.propValue - column's default character set: utf8
   querkiuser.uv7w4jd36.propValue - column's default character set: utf8
   querkiuser.uv7w4jd4p.propValue - column's default character set: utf8

   More information:
     https://dev.mysql.com/doc/refman/en/charset-unicode-utf8mb3.html


5) Table names in the mysql schema conflicting with new tables in the latest
MySQL. (mysqlSchema)
   No issues found

6) Partitioned tables using engines with non native partitioning
(nonNativePartitioning)
   No issues found

7) Foreign key constraint names longer than 64 characters (foreignKeyLength)
   No issues found

8) Usage of obsolete MAXDB sql_mode flag (maxdbSqlModeFlags)
   No issues found

9) Usage of obsolete sql_mode flags (obsoleteSqlModeFlags)
   The following DB objects have obsolete options persisted for sql_mode.

   Warning: Ensure the following flags are not persisted in the configuration
   file as they will prevent the target server from loading.
   - @@global.sql_mode: defined using obsolete NO_AUTO_CREATE_USER option

   More information:
     https://dev.mysql.com/doc/refman/8.0/en/mysql-nutshell.html#mysql-nutshell-removals


10) ENUM/SET column definitions containing elements longer than 255 characters
(enumSetElementLength)
   No issues found

11) Usage of partitioned tables in shared tablespaces
(partitionedTablesInSharedTablespaces)
   No issues found

12) Circular directory references in tablespace data file paths
(circularDirectory)
   No issues found

13) Usage of removed functions (removedFunctions)
   No issues found

14) Usage of removed GROUP BY ASC/DESC syntax (groupbyAscSyntax)
   No issues found

15) System variable check for deprecation, removal, changes in defaults values
or invalid values. (sysVars)
  To run this check requires full path to MySQL server configuration file to be specified at 'configPath' key of options dictionary

16) Zero Date, Datetime, and Timestamp values (zeroDates)
   No issues found

17) Schema inconsistencies resulting from file removal or corruption
(schemaInconsistency)
   No issues found

18) Tables recognized by InnoDB that belong to a different engine (engineMixup)
   No issues found

19) Issues reported by 'check table x for upgrade' command (checkTableCommand)
   No issues found

20) New default authentication plugin considerations
(defaultAuthenticationPlugin)
   Warning: The default authentication plugin 'caching_sha2_password' offers
      more secure password hashing than previously used 'mysql_native_password'
      (and consequent improved client connection authentication). However, it also
      has compatibility implications that may affect existing MySQL installations.
       If your MySQL installation must serve pre-8.0 clients and you encounter
      compatibility issues after upgrading, the simplest way to address those
      issues is to reconfigure the server to revert to the previous default
      authentication plugin (mysql_native_password). For example, use these lines
      in the server option file:
      
      [mysqld]
      default_authentication_plugin=mysql_native_password
      
      However, the setting should be viewed as temporary, not as a long term or
      permanent solution, because it causes new accounts created with the setting
      in effect to forego the improved authentication security.
      
      MySQL 8.4.0 removes the deprecated default_authentication_plugin option.
      The deprecated mysql_native_password authentication plugin is disabled by
      default as of  MySQL 8.4.0, and is subject to removal in a future version.
      
      If you are using replication please take time to understand how the
      authentication plugin changes may impact you.
   More information:
     https://dev.mysql.com/doc/refman/8.0/en/upgrading-from-previous-series.html#upgrade-caching-sha2-password-compatibility-issues
     https://dev.mysql.com/doc/refman/8.0/en/upgrading-from-previous-series.html#upgrade-caching-sha2-password-replication
     https://dev.mysql.com/doc/refman/8.4/en/mysql-nutshell.html

21) Indexes on functions with changed semantics
(changedFunctionsInGeneratedColumns)
   No issues found

22) Columns which cannot have default values (columnsWhichCannotHaveDefaults)
   No issues found

23) Check for invalid table names and schema names used in 5.7 (invalid57Names)
   No issues found

24) Check for deprecated usage of single dollar signs in object names
(dollarSignName)
   No issues found

25) Check for indexes that are too large to work on higher versions of MySQL
Server than 5.7 (indexTooLarge)
   No issues found

26) Check for deprecated '.<table>' syntax used in routines.
(emptyDotTableSyntax)
   No issues found

27) MySQL syntax check for routine-like objects (syntax)
   No issues found

28) Check for columns that have foreign keys pointing to tables from a
different database engine. (invalidEngineForeignKey)
   No issues found

29) Check for deprecated or invalid user authentication methods.
(authMethodUsage)
   Some users are using authentication methods that may be deprecated or
   removed, please review the details below.

   Warning: The following users are using the 'mysql_native_password'
   authentication method which is deprecated as of MySQL 8.0.34 and will be
   removed in a future release.
   Consider switching the users to a different authentication method (i.e.
   caching_sha2_password).
   The 'mysql_native_password' authentication type is disabled by default in
   MySQL 8.4, but can still be enabled by setting
   loose_mysql_native_password=ON.
   - jducoeur@localhost
   - mysql.session@localhost
   - mysql.sys@localhost
   - root@localhost

   More information:
     https://dev.mysql.com/doc/refman/en/caching-sha2-pluggable-authentication.html



30) Check for deprecated or removed plugin usage. (pluginUsage)
   No issues found

31) Check for deprecated or invalid default authentication methods in system
variables. (deprecatedDefaultAuth)
   The following variables have problems with their set authentication method:

   Warning: default_authentication_plugin - mysql_native_password
      authentication method is deprecated and it should be considered to correct
      this before upgrading to 8.4.0 release.



32) Check for deprecated or invalid authentication methods in use by MySQL
Router internal accounts. (deprecatedRouterAuthMethod)
   No issues found

33) Check for deprecated temporal delimiters in table partitions.
(deprecatedTemporalDelimiter)
   No issues found
Errors:   0
Warnings: 77
Notices:  0

NOTE: No fatal errors were found that would prevent an upgrade, but some potential issues were detected. Please ensure that the reported issues are not significant before upgrading.
```

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
