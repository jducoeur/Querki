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

**Don't linger on 8.0.** The upgraded test instance reports `VERSION()` = `8.0.46-eol-2-rds.20260908`.
The `eol` marks a post-end-of-life build: MySQL 8.0's community support ended April 2026 and, as far as
we know, RDS standard support for 8.0 has ended too. So 8.0 is probably in RDS **Extended Support**, as
5.7 was, with a per-vCPU-hour surcharge (confirm in the RDS console). Treat 8.0 as a brief waypoint:
in each environment, do the 8.0 → 8.4 hop soon after 5.7 → 8.0 has validated. For prod in particular,
schedule the two hops close together, in separate maintenance windows rather than months apart.

## Progress

**Test environment: on 8.0 as of 2026-11-01.**
- [x] Pre-upgrade check against 5.7 (0 errors; results below).
- [x] Built `querki-test-mysql80` parameter group.
- [x] Upgraded to `8.0.46-rds.20260908` (engine reports `8.0.46-eol-2-rds.20260908`). Post-upgrade
      values confirmed: `sql_mode=NO_ENGINE_SUBSTITUTION`, `character_set_server=utf8mb3` (8.0's name for
      `utf8`), `max_allowed_packet=1073741824`.
- [ ] Parameter group shows in-sync; app reconnected; smoketests run on 8.0.
- [ ] **Next session: 8.0 → 8.4.**
  1. Convert `querki@%` and `admin@%` to `caching_sha2_password` (see Authentication). Use each
     account's existing password. Afterwards, confirm the app still connects; this is the TLS test.
  2. Re-run the pre-upgrade check with `--target-version=8.4.x`. You'll need `mysqlsh` again:
     CloudShell VPC environments lose their home directory, so re-download it and `global-bundle.pem`.
     An older MariaDB `mysql` client may not authenticate once `admin` is on `caching_sha2_password`.
  3. Build `querki-test-mysql84` (family `mysql8.4`) with the same pins: check `sql_mode` per-parameter,
     `character_set_server=utf8mb3`, `collation_server=utf8mb3_bin`, `max_allowed_packet`. No
     `default_authentication_plugin` (removed in 8.4).
  4. Manual snapshot, then Modify → 8.4 with the new parameter group and the `default:mysql-8-4`
     option group.
  5. Post-upgrade: version and parameter checks, app reconnects, smoketests.
  6. Then remove the temporary `cloudshell-dbcheck` inbound rule from the RDS security group.

**Prod:** not started.

## What must be true before touching RDS

- **Code is already deployed** carrying the Connector/J 8.0.33 driver and the driver-compat fixes
  (the `LocalDateTime` converter, the `SQLSyntaxErrorException` catch). The driver bump is the risky
  part and should already be proven in that environment against 5.7. See the plan doc.
- **The pre-upgrade check has been run** against that environment's 5.7 data and shows no fatal errors.
  We ran it locally (results in `local_mysql_upgrade.md`) — but **re-run it per environment**, because
  test and prod data differ from your laptop and from each other. Point MySQL Shell at the RDS endpoint,
  over verified TLS. That needs AWS's public RDS CA bundle (not a secret; one global bundle covers all
  regions):
  ```bash
  curl -o global-bundle.pem https://truststore.pki.rds.amazonaws.com/global/global-bundle.pem
  mysqlsh --ssl-mode=VERIFY_IDENTITY --ssl-ca=./global-bundle.pem \
    -- util check-for-server-upgrade admin@<rds-endpoint>:3306 --target-version=8.0.46
  ```
  (Test endpoint: `ae-test-querki-db.<endpoint id from AWS console>.us-east-1.rds.amazonaws.com`; master user `admin`.)
  `--ssl-mode=REQUIRED` would skip the CA check, but only use it as a fallback.
  Expect the same three warning classes we saw locally (auth method, `NO_AUTO_CREATE_USER`, `utf8mb3`)
  — none fatal — but confirm nothing new appears on the larger prod dataset.

### Reaching the RDS instance (it's private)

The RDS endpoint is **not publicly accessible**: its hostname resolves to a private `10.x` address
inside the VPC, so a laptop has no route to it. Trying to connect directly fails fast with:
```
ERROR: MySQL Error 2003 (HY000): Can't connect to MySQL server on '<rds-endpoint>:3306' (60)
```
Error 2003 means no TCP connection was ever made, and `(60)` is macOS `ETIMEDOUT`. It's reachability,
not credentials (that would be 1045) or TLS (2026). Confirm with `dig +short <rds-endpoint>`: a
`10.x` answer means private. Don't make the instance public just to run a check. Pick one of these
instead (roughly in order of preference):

1. **SSM port forwarding** through any EC2 host in the VPC that runs the SSM agent (an app host is
   fine). Nothing gets opened up:
   ```bash
   aws ssm start-session --target <instance-id> \
     --document-name AWS-StartPortForwardingSessionToRemoteHost \
     --parameters host=<rds-endpoint>,portNumber=3306,localPortNumber=3306
   ```
   Then, in another terminal, point `mysqlsh` at `127.0.0.1:3306` with **`--ssl-mode=VERIFY_CA`**
   instead of `VERIFY_IDENTITY`. The certificate is issued for the RDS hostname, not `127.0.0.1`, so the
   hostname check would fail even though the connection is fine. `VERIFY_CA` still validates the
   certificate against `global-bundle.pem`.
2. **AWS CloudShell in a VPC environment.** Launch it into the RDS instance's VPC and subnet, install
   MySQL Shell, upload `global-bundle.pem`, and run the check against the real hostname with
   `VERIFY_IDENTITY`.
3. **SSH/SSM into a host in the VPC** and run `mysqlsh` there directly.

In every case the RDS security group must allow inbound 3306 from that source. App hosts presumably
already are, which is why tunnelling through an existing app host is usually the least work. The same
applies to prod.

What actually worked for test: our old bastion host no longer connects, and the app runs on Elastic
Beanstalk with no dedicated node, so we used **CloudShell in a VPC environment**:
- Create a security group `cloudshell-dbcheck` in the RDS instance's VPC with no inbound rules.
- On the RDS instance's security group, add an inbound rule: type `MYSQL/Aurora`, source
  `cloudshell-dbcheck`. Edit the rule in the EC2 console's Security Groups page; the RDS console only
  shows the rules.
- Create the CloudShell VPC environment with that VPC, subnet and security group.
- Install MySQL Shell from the generic Linux tarball, matching the local version:
  ```bash
  curl -L -o mysql-shell.tar.gz \
    'https://downloads.mysql.com/archives/get/p/43/file/mysql-shell-9.7.1-linux-glibc2.28-x86-64bit.tar.gz'
  tar xzf mysql-shell.tar.gz
  ```
  Use the `/get/` or `/archives/get/` direct URL with `curl -L`. The `dev.mysql.com/downloads/file/?id=…`
  links are HTML landing pages, and `tar` rejects them with "not in gzip format".
- Remove the temporary security group rule afterwards.

### Pre-upgrade check results: test environment (5.7.44-rds)

**0 errors, 73 warnings.** Compared with the local run:
- **utf8mb3 (check 4):** same story, different table list. Deferred to `charset_migration.md`.
- **Auth plugin (checks 20, 29, 31):** accounts on `mysql_native_password` are `admin@%` (master),
  **`querki@%` (the app account)**, and the locked reserved accounts `mysql.session` and `mysql.sys`
  (leave those alone). Convert `querki` and `admin` before 8.4; see Authentication below.
- **Obsolete `NO_AUTO_CREATE_USER` (check 9):** *not* present on RDS. Nothing to strip.
- **New, zero dates (check 16):** the **setting**, not the data, is flagged. No zero values were found,
  but `global.sql_mode` lacks `NO_ZERO_DATE`/`NO_ZERO_IN_DATE`, so the parameter group has a custom
  `sql_mode`. Not a blocker; see Parameter groups below.

Re-run this against prod before its upgrade. Prod's data, and possibly its parameter group, differ.

## RDS-specific mechanics (differ from local)

- **Parameter groups.** A DB parameter group is tied to a major version, so you cannot carry the 5.7
  group forward. Create an **8.0 parameter group** (and later an **8.4** one) mirroring your current
  custom settings, and attach it as part of each upgrade. While building them:
  - Do **not** carry `NO_AUTO_CREATE_USER` in `sql_mode` (removed; blocks startup).
  - `default_authentication_plugin` may be set in the 8.0 group but is **removed in 8.4** — don't put
    it in the 8.4 group.
  - **How to build one (first time):**
    1. Find the instance's current groups: RDS → Databases → instance → **Configuration** tab →
       "DB instance parameter group" and "Option group".
    2. List the parameters that differ from engine defaults (CloudShell has the AWS CLI):
       ```bash
       aws rds describe-db-parameters --db-parameter-group-name <current-group> --source user \
         --query 'Parameters[].[ParameterName,ParameterValue]' --output table
       ```
    3. RDS → **Parameter groups** → **Create parameter group**: engine *MySQL Community*, family
       `mysql8.0`, type *DB Parameter Group* (not cluster), name e.g. `querki-test-mysql80` (one per
       environment).
    4. Select it → **Edit**, set the values below, **Save changes**. Re-run step 2's command against
       the new group to verify.
    5. Don't attach it yet: an 8.0 group can't go on a 5.7 instance. Attach it on the **Modify** screen
       at the same time you change the engine version. Afterwards, the instance's Configuration tab
       should show the group as **in-sync**; if it says **pending-reboot**, reboot once.
  - **Test environment (done the first time):** it was on `default.mysql5.7` and `default:mysql-5-7`.
    The default group can't be edited, so its four "user"-source values are RDS's own 5.7 defaults:
    `character_set_server=utf8`, `collation_server=utf8_bin`, `innodb_log_file_size=671088640`,
    `max_allowed_packet=1073741824`. Pin the values that change behaviour in `querki-test-mysql80`:
    `sql_mode=NO_ENGINE_SUBSTITUTION`, `character_set_server=utf8`, `collation_server=utf8_bin`,
    `max_allowed_packet=1073741824`. Do **not** carry `innodb_log_file_size` over: it's a tuning knob,
    deprecated in 8.0.30+ in favour of `innodb_redo_log_capacity`, and RDS sizes it per instance class.
    The option group needs nothing; pick `default:mysql-8-0` on the Modify screen. **Re-do steps 1–2 for
    prod** rather than assuming it matches.
    - **`sql_mode` won't appear in the `--source user` check.** RDS's own `mysql8.0` default is already
      `NO_ENGINE_SUBSTITUTION`, so setting that value doesn't register as a change: the parameter keeps
      source `system`. That's fine, not a failed edit. Verify it per-parameter instead:
      ```bash
      aws rds describe-db-parameters --db-parameter-group-name querki-test-mysql80 \
        --query "Parameters[?ParameterName=='sql_mode'].[ParameterName,ParameterValue,Source]" --output table
      ```
      Expect `NO_ENGINE_SUBSTITUTION | system`. After hop 1, still confirm with `SELECT @@global.sql_mode;`.
  - **`sql_mode`: carry the current value forward unchanged** for the upgrade. Test's value is just
    **`NO_ENGINE_SUBSTITUTION`** (minimal, non-strict; as far as we know, the RDS 5.7 default). Set
    that **explicitly** in the custom 8.0 parameter group rather than relying on whatever default the
    group comes with, and confirm with `SELECT @@global.sql_mode;` after hop 1. If the instance is on
    AWS's `default.mysql5.7` group rather than a custom one, the upgrade attaches `default.mysql8.0`
    unless told otherwise, so a custom group is how you pin the value. Check prod's value too; it may
    differ.
  - This is why check 16 (zero dates) fired: non-strict mode allows zero dates. The data check found
    none, consistent with Querki never writing them.
  - **Local dev and RDS have always run different modes.** Local Homebrew 5.7 uses MySQL's stock strict
    default (`ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,
    ERROR_FOR_DIVISION_BY_ZERO,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION`), which is why only the local
    check flagged `NO_AUTO_CREATE_USER`. On RDS, `ONLY_FULL_GROUP_BY` isn't active, so the GROUP BY
    concern doesn't apply there while the value is kept.
  - **Later cleanup, separate from this upgrade:** move RDS to 8.0's strict default `sql_mode`. The app
    has effectively run under strict mode locally for years, which lowers the risk. The remaining
    unknown is production *data*: non-strict mode silently truncates or coerces bad values instead of
    rejecting them, so strict mode could surface errors the app currently gets away with.
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
-- Accounts confirmed by the test-environment check; re-check prod's list before converting there.
ALTER USER 'querki'@'%' IDENTIFIED WITH caching_sha2_password BY '<app pw>';    -- the app account
ALTER USER 'admin'@'%'  IDENTIFIED WITH caching_sha2_password BY '<master pw>'; -- RDS master user
SELECT user, host, plugin, account_locked FROM mysql.user;  -- confirm no real account is still on native_password
```
Use each account's **existing** password so nothing else needs changing. The app's password comes from
Secrets Manager in AWS.

Notes vs. local:
- On RDS the app connects from app hosts, so the grant host is typically `'%'` or a VPC range, **not**
  `@localhost`. Convert whatever host pattern the app account actually uses.
- Leave RDS-internal/`rdsadmin` and system accounts to AWS; only convert accounts you own.
- **TLS is probably already in use, implicitly.** The app's URL (in `test.conf`, not overridden in
  secrets) has no SSL parameters, but Connector/J 8.0.13+ defaults to `sslMode=PREFERRED`, which uses TLS
  whenever the server offers it, and RDS always does. So app connections should already be encrypted,
  though the certificate isn't verified. That's enough for `caching_sha2_password`, so no
  `allowPublicKeyRetrieval` flag should be needed on RDS. (Local differs because local MySQL has no TLS,
  so `PREFERRED` falls back to plaintext.)
  - To confirm, if `performance_schema` is enabled (often off on RDS 5.7):
    ```sql
    SELECT t.processlist_user, t.processlist_host, s.variable_value AS tls
      FROM performance_schema.threads t
      JOIN performance_schema.status_by_thread s ON s.thread_id = t.thread_id
     WHERE s.variable_name = 'Ssl_version' AND t.processlist_user = 'querki';
    ```
  - Otherwise the conversion itself is the test: if the app still connects after `querki` is converted
    on 8.0, TLS works. If it fails with a public-key error, add `sslMode=REQUIRED` to the URL.
- **Future hardening (not part of this upgrade):** set `sslMode=REQUIRED` explicitly in the test and
  prod configs so TLS can't silently fall back to plaintext. Full `VERIFY_IDENTITY` would also need the
  RDS CA bundle in a Java truststore and connecting by the real RDS hostname. The config uses a short
  alias, which won't match the certificate.

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
