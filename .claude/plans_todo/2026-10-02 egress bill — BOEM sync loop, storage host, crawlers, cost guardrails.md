# Egress bill (2026-10-02): the BOEM sync loop, the storage host, crawlers, and cost guardrails

Investigated 2026-10-02 (Fable 5.1) from AWS Cost Explorer, CloudWatch, the server's interface counters, the Caddy
storage log and the BOEM host's own sync log. Everything in §1 was measured today unless marked *inferred*.

## 0. One-paragraph state

**The $401 is not crawlers and not the storage proxy.** It is the BOEM internal server's hourly `rclone` pull
(`server/prod/sync-pull.sh`, key `msens-sync@boem`, from Azure). That host's disk has been full since
2026-08-27 17:05 UTC — the hour v9's per-model PMTiles appeared on the file host — and every hour since it re-reads
the same 3,950 files it cannot store. ~5.8 GB per hour, 24 times a day, ≈ 150 GB/day, ≈ $13/day, **still running**.
The storage proxy has served 163 MB in seven weeks (about one cent). The retooling Ben asked for (link out to S3,
crawler policy, usage tracking, one URL contract from ingest to Atlas) is still worth doing, but as exposure
reduction and hygiene, in that order after the meter is stopped and an alarm exists that would have caught this on
day one.

## 1. Evidence

| measurement | value | source |
| --- | --- | --- |
| EC2 `DataTransfer-Out-Bytes`, Sep 2026 | 4,518.9 GB = $397.70 | Cost Explorer |
| same, Aug 2026 | 702.8 GB = $55.60 (0.1–0.2 GB/day until Aug 26; 55.8 GB on Aug 27; 153–168 GB/day after) | Cost Explorer daily |
| S3 `DataTransfer-Out-Bytes`, Sep 2026 | 36.7 GB = $3.30 | Cost Explorer |
| daily shape | flat 150–154 GB every day for 35 days; dips only on the days the host was down (Sep 21, 24) | Cost Explorer daily |
| hourly shape | one 5-minute bucket of 5.6–6.0 GB per hour, ~0 otherwise | CloudWatch `NetworkOut`, 5-min |
| Caddy container, total sent in 13.4 h since boot | 583 MB (all vhosts together ≈ 1 GB/day ≈ $3/month) | `docker stats` |
| host `eth0` sent in the same 13.4 h | 79.7 GB | `/proc/net/dev` |
| storage proxy, 2026-08-10 → 10-02 | 8,506 requests, 162.9 MB total; 57 % are 404s | `/share/logs/caddy/storage.log` |
| BOEM cycle | `pulling pmtiles...` → 3 attempts, `Listed 155100`, `Errors: 3950`, `no space left on device`, 1m40s | `/share/logs/prod/sync-pull.log` |
| what fails | 3,950 files, all `pmtiles/v9/rng_iucn/`, 3.24 GB, none over 16 MiB | sizes summed on the server |
| first failure | `2026/08/27 17:05:49 … pmtiles/v9/rng_iucn/133534.pmtiles … no space left on device` | same log |
| BOEM host state | `ioemazeudmar01`: `disk_free: 20K`, `caddy: exited`, `shiny: exited` | `/share/logs/prod/heartbeat.json` |
| `=== sync-pull completed ===` | never logged, not once since 2026-02-20 | same log |
| the log itself | 2.09 GB, re-uploaded to msens1 every hour (29 GB received in 13 h) | `ls`, `/proc/net/dev` |

Mechanism (*inferred*, consistent with every number above): `rclone sync ext_dev:/share/data/derived … --include
"*.pmtiles"` opens each source file over SFTP and fills its read-ahead buffer (default 16 MiB, larger than any of
these files) before the local `.partial` open fails; three attempts per run; `set -e` then aborts the script, so the
`git pull` that could deliver a fixed script never runs. The host cannot heal itself.

Ruled out, with the measurement that rules it out:

- **Storage proxy / crawlers on it.** 163 MB in seven weeks. The crawlers that do visit (GPTBot 537 requests,
  ClaudeBot 818, Amazonbot 589, plus scanners asking for `.env` 1,937 times and `.php` 590 times) get 404s or
  small index pages. The one large day (2026-09-15, 143 MB) was user agent `node`, i.e. our own scripts.
- **Any Caddy-served host** (apps, titiler, file host, STAC): 583 MB in 13.4 h for all of them together.
- **The warm sidecar**: talks to `rstudio:3838/3839` inside the Docker network.

Not measured: per-host traffic on any vhost other than `storage` and `preview`. They have no access log. That is
a gap this plan closes (P5), not evidence of a problem.

Two facts about price that shape the design:

- EC2 egress and S3 egress cost the **same** $0.09/GB, sharing one 100 GB/month free allowance. Redirecting a
  download from the VM to S3 does not make it cheaper. It removes the VM as the chokepoint, stops paying twice in
  attention, and puts all download accounting in one place.
- What makes downloads cheaper is a CDN with free origin egress. Not needed at 37 GB/month. Designed for in P7.

Why nobody saw it for five weeks: the only budget is "$20/month" against a ~$200/month baseline, so it has been in
ALARM permanently. No Cost Anomaly monitor exists. `monitor-heartbeat.sh` (installed, root cron, every 10 min)
checks only that the heartbeat is fresh, and writes its alerts to a file.

## 2. Hard constraints

Inherited from the handoff (`2026-10-02 handoff — asset store migration…`), unchanged:

- No agent writes to the production bucket, the server, AWS account settings or `authorized_keys` unattended.
  Ben runs those steps with the `!` prefix, or grants a narrow permission rule. Marked **[Ben]** below.
- Every server change is a commit in `server/` deployed through `release_marine-atlas.qmd` (`DEPLOY_CADDY=1`),
  never a hand edit inside a container. The one exception in this plan is P0, which is host state no repo holds.
- Sonnet 5.5 builders, lean local gates, no reviewer agents, the orchestrator does not write feature code.
- `server` auto-commits and pushes during iteration; `workflows` and `msens` push only when Ben says.
- Every check added here gets a seeded fault: show it failing once before trusting it passing.

## 3. Decisions for Ben

| # | decision | recommendation |
| --- | --- | --- |
| D1 | Is the BOEM internal mirror still wanted? It syncs `DATA_VERSION=v4`, its services are down, and the product has since moved to one-app-every-version and the static Atlas. | Ask BOEM. Until answered, treat as "repair, but scoped" (P2). If retired: remove the key for good, delete `prod/`, close `sync.qmd` with a note. |
| D2 | P0 method: disable the key (stops pull, heartbeat and log push) or leave access and wait for BOEM to free disk. | Disable the key today. Their host is already down, so nothing working is lost, and every day of waiting is ~$13. |
| D3 | Should the BOEM key keep a full `ubuntu` shell on msens1? Today it has one, including the docker group. | No. P2 moves it to a dedicated SFTP-only account chrooted to an export tree. |
| D4 | File host: turn off `file_server browse` on `/derived/*` (81 GB) and `/pmtiles/*`? | Yes. Keep browse on `/stac/*` and `/branding/*`. The S3 index is the browsable front door. |
| D5 | Block AI crawlers on data hosts, allow them on catalog and docs pages? | Yes, as written in P5. |
| D6 | Ask AWS for a courtesy credit for Aug–Sep egress? | Worth one billing support case after P0 and P1 are done (they ask what was fixed). Not guaranteed. |

### Decided 2026-10-02 (Ben), and what it changed

- **D1: retire the mirror.** The static Atlas on GitHub Pages reading S3 removes the reason for it. Fix the
  immediate situation first, then pitch the way forward to Tim. P2 below shrinks to "disable the key, later
  remove `prod/` and the heartbeat cron"; no `boemsync` account, no script rewrite.
- **D4: yes.** Done in `server` `5f3ac69` (not yet deployed): no listings on `/pmtiles`, `/derived` or `/` of the
  file host, plus `robots.txt` and an access log there. `/stac/` and `/branding/` stay browsable.
- **D6: yes.** Draft and where to file it: `egress-refs/aws_credit_request.md`. It needs P1 applied first.
- **AquaX:** Ben wants to pitch skipping it (private grids cannot be kept private behind a static app on a
  public bucket). Draft to Tim: `egress-refs/email_tim_internal-mirror_aquax.md`, also a Gmail draft.

**No script can be delivered to the BOEM host.** Its copy of `sync-pull.sh` has never reached `git pull`: zero
`pulling app code`, `server repo updated` or `completed` lines in its log since 2026-02-20 (it dies at the
static-website step every run). The only thing it obeys is the source listing: `rclone sync` deletes a
destination file whose source is gone. So the fix is on our side, and it replaces P0:

1. `server/host/pmtiles_native.sh link` hardlinks `/share/data/derived/pmtiles/{v8,v9}` to
   `/share/data/pmtiles_native/` (v9 is itself hardlinks of v8: 6.7 GB once here, twice on the mirror).
2. Caddy roots `/pmtiles/v8/*` and `/pmtiles/v9/*` there, so the 6,753 PMTiles URLs in each of the published v8
   and v9 `native_asset` tables keep resolving.
3. `pmtiles_native.sh retire` removes the two trees from `derived/` only after the public URL answers, and
   puts them back if it stops answering.
4. At the mirror's next `:05` run there is nothing to copy, so there are no errors, so it deletes its v8 copy
   (6.7 GB) and its partial v9 (2,826 files): about 10 GB freed, egress back to a directory listing.

Steps 1–3 are wired into the `deploy-caddy` chunk of `release_marine-atlas.qmd` (uncommitted in `workflows`);
both script calls are no-ops afterwards. **The key must stay enabled until step 4 is observed**
(`heartbeat.json` `disk_free` rising from 20K), then disable it with the P0 one-liner. Tested: the file vhost
(real Caddyfile text, 23 assertions, 13 fail on the previous Caddyfile) and the move script (six cases on a
fixture tree, including "caddy still on the old root" → removal detected and restored).

Expected on the mirror, not verified until it runs: that its `rclone` deletes (it will if nothing errors), and
that its `caddy`/`shiny` stay exited (nothing restarts them; that is BOEM IT's call now).

## 4. Phases

Order: P0 today, P1 this week, then P2 and P4–P5 in parallel (different files), P3 and P6 ride on the asset-store
migration, P7 only on its trigger.

### P0. Stop the meter **[Ben]**, 5 minutes

```bash
# disable (reversible: the .bak is the undo)
ssh msens "sed -i.bak-20261002 '/msens-sync@boem/s/^/# DISABLED 2026-10-02 egress loop: /' ~/.ssh/authorized_keys && grep -c '^# DISABLED' ~/.ssh/authorized_keys"
# verify after the next top of the hour: the 5-minute buckets should all be < 100 MB
aws cloudwatch get-metric-statistics --namespace AWS/EC2 --metric-name NetworkOut \
  --dimensions Name=InstanceId,Value=i-0692d15b330da30b6 --period 300 --statistics Sum \
  --start-time "$(date -u -v-2H +%FT%TZ)" --end-time "$(date -u +%FT%TZ)" \
  --query 'sort_by(Datapoints,&Timestamp)[].[Timestamp,Sum]' --output text
```

Pass = no bucket above 100 MB across a full hour, and `sudo journalctl -u ssh --since -15min | grep 20\\.` shows
the Azure addresses being refused. Then tell the BOEM host admin, with these facts: disk at 20 K free since
Aug 27; `caddy` and `shiny` exited; `sync-pull.log` is 2 GB and has been uploaded hourly; the pull has never
completed a cycle since February; access is paused on our side until the script is fixed (P2).

### P1. Guardrails: no surprise can run longer than a day. One Sonnet builder; **[Ben]** applies

New committed, idempotent script `server/aws/guardrails.sh` (same style as `server/cloudflare/access.sh`: reads
`.env`, converges, prints what it changed), plus `server/aws/README.md`.

1. SNS topic `msens-alerts` with Ben's email subscribed.
2. CloudWatch alarms on `AWS/EC2 NetworkOut` for msens1: sum over 1 h > 3 GB, and sum over 24 h > 15 GB.
   Either one would have fired on 2026-08-27.
3. Cost Anomaly Detection: one service monitor, daily email at ≥ $5 impact.
4. Budgets: replace the $20 budget with a total budget at the real baseline (+15 %), alerts at 80/100 %
   actual and 100 % forecast; add a second budget filtered to usage type `DataTransfer-Out-Bytes` at $10.
5. S3 request metrics: one filter on prefix `marine-atlas/`, alarm on `BytesDownloaded` > 20 GB/day.
6. S3 server access logging for `oceanmetrics.io-public` to a new private bucket with a 90-day expiry.
   This is the answer to "can we track S3 usage": yes, per object, per requester IP and user agent.

Gate: run with thresholds temporarily at 1 MB to see one alarm email arrive (seeded fault), then converge to the
real values. `guardrails.sh --check` exits non-zero if any of the six is missing.

### P2. The sync: scope it, bound it, make it report. One Sonnet builder on `server/prod/`; applies only if D1 = keep

- `sync-pull.sh`: run the `git pull` of `server` **first**, so a script fix can always land. Drop `set -e` for the
  transfer steps (each logs its own exit status). Pull PMTiles from `pmtiles/${DATA_VERSION}` only, never the
  whole `derived/` tree. Precheck free disk and skip the transfers below a floor. `--retries 1`,
  `--max-transfer 5G --cutoff-mode hard`, `--low-level-retries 1`.
- Logs: rotate at 20 MB, keep 5. `sync-push.sh` pushes the rotated tail, not a growing file.
- `ping.sh` already reports `disk_free` and service state. `monitor-heartbeat.sh` must act on them: alert when
  disk is under a floor, a service is not running, the last pull had errors, or the heartbeat is stale, and
  publish to the P1 SNS topic instead of appending to a file.
- D3: account `boemsync`, `ForceCommand internal-sftp`, chrooted to `/share/export/boem/` holding only what the
  internal stack serves. Document in `sync.qmd`. The key moves there; it leaves `ubuntu`'s `authorized_keys`.
- Tests: `prod/test/run.sh` with a tmpfs destination too small for the source proves the run stops at the
  precheck and transfers nothing (the fault that cost $450), and a second case proves `--max-transfer` cuts off.

Re-enable the key only after BOEM has freed disk and pulled the new script.

### P3. Remove the third copy of the PMTiles from the file host. This is asset-store M6, unchanged

`/share/data/derived/pmtiles/{v8,v9}` (2 × 6,776 files, 6.7 GB) is what the BOEM loop was reading. The asset-store
plan already deletes it after the two-week soak, once pointers and STAC name the S3 store. No change to that
sequence. P2's scoping means it no longer matters to the sync in the meantime.

### P4. Storage host: serve the pages, redirect the bytes. One Sonnet builder; `server` + `msens` + `workflows`

- `server/caddy/Caddyfile`, storage vhost: reverse-proxy only directory URLs (rewritten to `index.html`),
  `index.html`, `README.md` and `robots.txt`. Everything else under the allow-list answers
  `302 https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public{uri}`. The `backups/` restriction stays as is.
- `server/caddy/test/run.sh`: a directory URL is 200 HTML; an object URL is 302 with the S3 `Location`; a range
  request on an object is 302, not 206; `/backups/x` is 404. Seeded fault: point the test at the current
  Caddyfile and watch the object assertions fail.
- `msens::build_storage_index()` already links every file at `obj_url` (S3) and every folder at `site_url`.
  Add the test that pins it: no file row may carry `site_url`. Add `ga4_id = NULL` to `storage_page()`; when
  set it emits the standard gtag snippet with `content_group = "storage"` (the convention from the analytics
  rollout, `G-9HW6L751XG`). Version bump, `NEWS.md`, reinstall.
- `workflows/publish_storage_index.qmd`: pass the GA4 id; `data/storage_readme.yml`: say plainly that files
  download from S3 and folders are browsed here. Republishing the index pages is a bucket write: **[Ben]**.

GA4 will count people browsing pages. It cannot see crawlers or object downloads; those come from the Caddy log
and the S3 access log (P1 item 6, P5).

### P5. See every host, then set the crawler policy. One Sonnet builder on `server/caddy/`

- Snippet `(access_log)` imported by every vhost: JSON to `/share/logs/caddy/{host}.log`, same rotation as
  storage. Today only two of ~20 vhosts log.
- Snippet `(robots)`: `robots.txt` on every host. Data hosts (`file`, `titiler`, `titiler-v8`, `titilecache`,
  `tile`, `tilecache`, `pmtiles`, `h3t`, `h3tcache`, `api`, `stac-api`): `Disallow: /`. Catalog hosts
  (`storage`): pages allowed, data extensions disallowed (already so).
- Snippet `(no_ai_bots)` on the data hosts: 403 for user agents matching GPTBot, ClaudeBot, Claude-User,
  Amazonbot, Bytespider, CCBot, PerplexityBot, Google-Extended, meta-externalagent, and the SEO crawlers
  (AhrefsBot, SemrushBot, MJ12bot, DotBot). Not applied to app, docs or storage pages: being findable there
  is wanted.
- D4: remove `browse` from `/derived/*` and `/pmtiles/*` on the file host. An 81 GB open directory listing with
  no log and no robots.txt is the real crawl exposure on this VM, far more than the storage vhost.
- `workflows/scripts/egress_report.R` (committed, DuckDB over the Caddy JSON logs and the S3 access logs): bytes
  by host × path class × user-agent class per day, top 20 requesters. Run it after 7 days of logs.

Decision gate after the first report: if any host shows sustained crawler bytes that robots.txt and the UA block
did not stop, put that hostname behind Cloudflare (already the DNS provider) with caching and its managed
AI-bot rule. Not before; proxying titiler through Cloudflare changes cache behaviour the apps depend on.

### P6. One URL contract, ingest → msens → Atlas → STAC. Folds into asset-store M5; one Sonnet builder per repo

Rule: **bulk bytes (`.tif`, `.pmtiles`, `.parquet`, `.gpkg`, `.duckdb`) are fetched from the object store named by
one base URL; the VM serves only computed responses (tiles, API, apps) and small HTML/JSON.**

- `msens`: one source for the bases. Today `stac.R` defaults `data_base` and `file_base` to
  `file.marinesensitivity.org`, `storage.R` and `stac.R` each spell out the S3 base, `viz.R` the tiler. Add
  `atlas_bases()` (overridable by option/env) and `url_audit(urls)`, which classifies each URL as `store`,
  `computed`, `page` or `vm_bulk`. Test: one fixture URL per class, and `vm_bulk` for
  `file.marinesensitivity.org/pmtiles/v9/x.pmtiles`.
- Publish gate, extending the M5 gate already planned ("no `.tif`/`.pmtiles` under `{ver}/`, every pointer in
  the catalog"): `url_audit()` over `native_asset`, the app bundle shards, `manifest.json` and every STAC
  href must return zero `vm_bulk`. Call it from `publish_native.qmd`, `build_app_bundle.qmd`,
  `publish_stac_api.qmd` and `backfill_versions.qmd`.
- STAC rebuild (handoff §5 D): today v7 carries 275 hrefs at `file.marinesensitivity.org/cog/sdm` and 9 at
  `/derived/v7`; v9 carries 9 at `/pmtiles/v9`. After the migration they resolve to `cog/{grid}/{hash}.tif` and
  `native/{ds}/{hash}.pmtiles` on S3. The catalog JSON itself stays at `file.marinesensitivity.org/stac/` (it is
  kilobytes) and `stac-api` is unchanged. Gate: the same `url_audit()` over a full crawl of the static tree.
- `workflows` notebooks that still name the file host for data (`publish_native.qmd`, `backfill_versions.qmd`,
  `ingest_sdm-gm.qmd`, `ingest_sdm-nc.qmd`, `publish_stac_api.qmd`): originals go to `native/{ds}/` on S3 only.
- `atlas`: already correct in `src/` (data from the S3 base in `src/lib/release/dataBase.ts` and `index.html`,
  tiles from `titiler-v8`). Two files under `atlas/scripts/` name the file host: check and fix. Add one hermetic
  spec that fails on any request to `file.` or `storage.` during a species-lens session. Push is routine.
- `apps` (Shiny, being retired): five files name the file host. Leave them; note it in the retirement.
- `docs`: `server.qmd` and the downloads chapter describe the proxy. Update with the asset-store docs branch,
  held until live, fact-checked against the deployed Caddyfile.

### P7. Contingency, not scheduled: a CDN in front of the bucket

Trigger: the P1 alarm on S3 `BytesDownloaded`, or S3 egress above ~80 GB in a month. Then either CloudFront in
front of the bucket (S3 → CloudFront transfer is free; check the current free allowance at decision time) or a
move to an egress-free store. Because P6 leaves one base URL, the switch is a republish of pointers, bundles and
STAC, not a code hunt. "storage" was named that way for this reason.

## 5. Agent plan

| wave | agent | model | repo | deliverable | who applies |
| --- | --- | --- | --- | --- | --- |
| 0 | — | — | host | P0 one-liner | Ben |
| 1 | builder A | Sonnet 5.5 | `server` | `aws/guardrails.sh` + README (P1) | Ben runs it |
| 1 | builder B | Sonnet 5.5 | `server` | Caddy: storage redirect, snippets, file-host browse off, tests (P4 server half, P5) | Ben, `DEPLOY_CADDY=1` from the laptop |
| 1 | builder C | Sonnet 5.5 | `msens` | storage index test + `ga4_id`; `atlas_bases()`, `url_audit()` + tests; version, NEWS | orchestrator merges, reinstalls |
| 2 | builder D | Sonnet 5.5 | `server` | `prod/` sync rewrite + test, heartbeat alerts, `boemsync` account doc (P2) | Ben + BOEM admin |
| 2 | builder E | Sonnet 5.5 | `workflows` | publish gates wired to `url_audit()`, `egress_report.R`, storage index notebook (P4, P5, P6) | Ben for any bucket write |
| 3 | builder F | Sonnet 5.5 | `atlas` | scripts fix + no-VM-bulk spec (P6) | push is routine |
| 3 | sweep | Haiku | `docs` | server and downloads chapters | hold until live |

A and B touch different directories of `server` and can run together; B and D both edit `server` but different
trees. C must land before E. Wave 3 waits for the asset-store publish (handoff §5 A).

## 6. Done means

1. Daily EC2 egress under 2 GB for seven consecutive days (Cost Explorer), with the BOEM key either retired or
   re-enabled under P2.
2. `guardrails.sh --check` passes, and the seeded alarm email was received.
3. `curl -sI https://storage.marinesensitivity.org/marine-atlas/latest.txt` is a 302 to S3; the folder URL is 200.
4. Every vhost writes an access log; the first `egress_report` exists and its decision (Cloudflare or not) is
   recorded here.
5. `url_audit()` returns zero `vm_bulk` over every published pointer table, bundle, manifest and the STAC tree.
6. The October bill's egress line is under $20, almost all of it from Oct 1–2.

## 7. Addendum (Ben, 2026-10-02): the preview gate stays; it is not a data-confidentiality control

The sign-in preview host (`preview.marinesensitivity.org`, Cloudflare Access, one policy per restricted version)
continues: it is how the framework is developed internally and how each new dataset is checked with its
provider before release. That is a small lift and it works. It keeps an unfinished release out of public view.
It does **not** keep a release's files private: they sit in the public bucket, readable by anyone holding the
URL. Restricting access to source data files (AquaX `*.tif`) would need a private store plus signed URLs or an
authenticating proxy in front of every tile and range request, which is the server the static Atlas was built
to avoid.

Consequences for this plan:

- **Rule for P6:** only data that may be public goes into the bucket, whatever the release's `access`. The
  publish gate gains one check: every `dataset` in a release carries a licence flag that permits public
  hosting, or the publish fails.
- **AquaX, if Tim agrees:** remove `ax` and `ax_native` objects from `v9/native/` (and from the asset-store
  copy plan before the migration writes them into `cog/global05/` and `native/ax/`), drop `ax` from the v9
  bundle and STAC, and keep `data/ax_vs_am_summary.csv` plus the local rasters as the internal benchmark.
  **This interacts with the staged asset-store migration: decide before running its Step 3**, or the AquaX
  files get a second, content-addressed public copy that then has to be hunted down.
- Bucket writes and deletes stay Ben's steps.

## 8. Outcome of the source-side fix (2026-10-02)

`DEPLOY_CADDY=1` ran at 09:17 UTC (`server` `5f3ac69`). Verified: v8/v9 PMTiles out of `derived/` (0 left, 6,776
each under `pmtiles_native`), eight published pointer URLs answer 206, listings 404 on `/pmtiles/`, `/derived/`
and `/`, `robots.txt` and `file.log` present. The mirror's 10:05 UTC run sent 4.6 MB in its five-minute window
(5,844 MB and 5,904 MB the two hours before); its 10:10 heartbeat reports `disk_free` 11G (was 20K). Remaining
from this plan: disable the key (P0 one-liner), P1, P4–P7, and the AquaX decision before asset-store Step 3.

## 9. Status after the first two agent waves (2026-10-02, afternoon)

Oversight: Fable 5.1. Builders: Sonnet 5.5, one per repo, no commits/pushes/deploys by builders; every diff
reviewed and every test re-run by the oversight session before it was committed.

| phase | state | where |
| --- | --- | --- |
| P0 stop the meter | DONE: mirror's disk 11G free, egress 4.6 MB in the next hourly window, key disabled 10:18 UTC | `server` `5f3ac69`, deployed |
| P1 guardrails | built, reviewed, pushed; **apply is Ben's** (`ALERT_EMAIL=… aws/guardrails.sh --apply`, then `--test-alarm`, `--check`) | `server` `1686536` |
| P2 mirror | retired (key disabled). Later: remove `prod/`, the heartbeat cron on msens1, `/share/logs/prod` | pending BOEM |
| P3 file-host PMTiles copy | unchanged: asset-store M6 | — |
| P4 storage host redirects objects to S3 | built, reviewed, pushed; **deploy is Ben's** (`DEPLOY_CADDY=1`) | `server` `f301f9f` |
| P5 logs on all 18 vhosts, robots, AI/SEO crawler 403 on data hosts | same commit, same deploy | `server` `f301f9f` |
| P5 egress report | `scripts/egress_report.R`; run after 7 days of logs, then decide on Cloudflare | `workflows` `bbaf71a3` |
| P6 one base-URL source + audit | `msens` 0.51.0 (`atlas_bases`, `url_audit`, `url_audit_assert`), committed locally, installed on the laptop | `msens` `52446b3` |
| P6 publish gate | `libs/url_gate.R` in five notebooks: FAILS on published v9 (6,753), PASSES on all four staged tables | `workflows` `bbaf71a3` |
| STAC alias at marinesensitivity.org/stac/ | live; regenerated by the pipeline (`libs/stac_alias.R`, `STAC_ALIAS_PUSH=1`) | site `55a8fb6`, `msens::stac_catalog_alias()` |
| Atlas follow-ups (handoff §6 items 1–4) | built on branch `r5-followups` (5 commits), held off `main` | atlas worktree |

Open, in order:

1. Ben: `DEPLOY_CADDY=1`; guardrails `--apply`; AWS credit case (draft in `egress-refs/`).
2. Ben: the asset-store runbook. The new gate is on its path and passes on the staged tables; a build of
   v8/v9 from the PUBLISHED tables now stops by design (the runbook already passes
   `APP_BUNDLE_NATIVE_ASSET_DIR=…/publish_stage`).
3. After the store is live (handoff §5 D): `msens::stac_build()` still emits `pmtiles_native` hrefs that are
   file-host DIRECTORIES (8 in v8, 9 in v9) — `vm_bulk`, and 404 since listings were turned off. Decide what a
   dataset-level PMTiles asset points at when each model is its own object, fix it in `R/stac.R`, gate the
   `stac` chunk of `release_marine-atlas.qmd`, add the gate before `backfill_versions.qmd`'s tables push, then
   rebuild STAC. Then the M5 guard in `publish_native.qmd`.
4. Atlas: merge `r5-followups` (bump version, rename `# Unreleased` in CHANGELOG) when Ben says; it deploys.
5. gm/nc ingests (handoff §7): independent, but heavy on the laptop; do not run alongside the runbook.
6. Not pushed: `workflows` (now 35 ahead), `msens` (13 ahead). The server container converges msens from
   its `/share` checkout, so 0.51.0 reaches the server only after a push and a pull there.

## 10. P1, P4, P5 closed (2026-10-02, 14:00 local)

- `DEPLOY_CADDY=1` ran at 11:07 UTC (`server` `1686536`): storage objects 302 to S3 (Range included), all 18
  access logs exist, data hosts answer 403 to GPTBot and 200/`Disallow: /` on `robots.txt`, browsers and
  `Claude-User` pass; titiler `/cog/info`, file-host PMTiles (206), apps, STAC and the API all still answer.
  `tile.marinesensitivity.org` is 502 to a browser — its upstream container is not running (pre-existing).
- `aws/guardrails.sh --apply` converged all six guardrails; `--check` exits 0; a `--test-alarm` email arrived.
  Lesson recorded in `server` `49576de`/`76c11b6`: the SNS subscription was unsubscribed twice within minutes of
  a browser confirmation (something followed the "unsubscribe" link on AWS's confirmation page) and the first
  check still printed `ok`. Only a CONFIRMED subscription counts now, and it was confirmed with
  `--confirm` (AuthenticateOnUnsubscribe), so a link can no longer remove it.
- The daily network alarm and the $10 data-transfer budget are in ALARM for leftovers of Oct 1–2 (197 GB,
  ~$18); the monthly forecast email ($506) is the same two days extrapolated.
- Next for Ben: file the AWS credit case (`egress-refs/aws_credit_request.md`, ~$471); optional: delete the
  old $20 budget (`server/aws/README.md`).
