#!/usr/bin/env python3
"""store_copy.py — the asset-store migration's server-side copies, from ONE process.

The runbook's 10_copy.sh starts a new `aws s3 cp` process per object (16 at a time). Measured on
2026-10-02 that made 365 copies a minute on a busy laptop — 83,417 copies would have taken 3.8 h,
almost all of it Python start-up. This does the same S3 CopyObject calls (same source, same key,
metadata copied with the object) from a thread pool over one connection pool.

  python scripts/store_copy.py <store_plan.parquet> [--workers 48] [--dry-run]

Rules:
  - reads the plan's `action == 'copy'` rows: `src_key` (bucket-relative) -> `marine-atlas/{key}`;
  - lists what is already under the destination prefixes and SKIPS a key that exists with the
    planned size (so it resumes, and it never rewrites an object: with bucket versioning on, a
    rewrite would leave a second version behind);
  - a key that exists with a DIFFERENT size is a conflict: reported, never overwritten, exit 1;
  - sources flagged `src_multipart` (> 8 MiB) are left to `aws s3 cp`, exactly as 10_copy.sh would
    have copied them, so their ETags come out the same as the reviewed path: it prints the pairs to
    `<log dir>/store_copy_multipart.tsv` and the caller runs the CLI on that file;
  - every failure is a `FAILED <src> <dst> <error>` line in _output/logs/store_copy.log (the name
    and the marker 10_copy.sh used); exit status is non-zero if there is any.
Needs boto3 and duckdb's CLI on PATH (to read the parquet without pyarrow).
"""
import argparse, csv, io, os, subprocess, sys, threading, time
from concurrent.futures import ThreadPoolExecutor, as_completed

import boto3
from botocore.config import Config

BUCKET = "oceanmetrics.io-public"
ROOT = "marine-atlas/"
LOG = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "_output", "logs", "store_copy.log")


def plan_rows(plan):
    q = (f"COPY (SELECT key, src_key, CAST(bytes AS BIGINT) AS bytes, src_multipart "
         f"FROM read_parquet('{plan}') WHERE action = 'copy') TO '/dev/stdout' (FORMAT csv, HEADER)")
    out = subprocess.run(["duckdb", "-c", q], check=True, capture_output=True, text=True).stdout
    return list(csv.DictReader(io.StringIO(out)))


def existing(s3, prefixes):
    have = {}
    for p in prefixes:
        for page in s3.get_paginator("list_objects_v2").paginate(Bucket=BUCKET, Prefix=p):
            for o in page.get("Contents", []):
                have[o["Key"]] = o["Size"]
    return have


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("plan")
    ap.add_argument("--workers", type=int, default=48)
    ap.add_argument("--dry-run", action="store_true")
    a = ap.parse_args()

    rows = plan_rows(os.path.expanduser(a.plan))
    s3 = boto3.client("s3", config=Config(max_pool_connections=a.workers + 8,
                                          retries={"max_attempts": 8, "mode": "adaptive"}))
    prefixes = sorted({ROOT + r["key"].split("/")[0] + "/" + r["key"].split("/")[1] + "/" for r in rows})
    have = existing(s3, prefixes)
    print(f"plan: {len(rows):,} copies into {len(prefixes)} prefixes; already there: {len(have):,} objects", flush=True)

    todo, multipart, skipped, conflicts = [], [], 0, []
    for r in rows:
        dst = ROOT + r["key"]
        if dst in have:
            if have[dst] == int(r["bytes"]):
                skipped += 1
            else:
                conflicts.append((dst, have[dst], r["bytes"]))
            continue
        (multipart if r["src_multipart"] == "true" else todo).append((r["src_key"], dst))

    os.makedirs(os.path.dirname(LOG), exist_ok=True)
    mp_file = os.path.join(os.path.dirname(LOG), "store_copy_multipart.tsv")
    with open(mp_file, "w") as f:
        for src, dst in multipart:
            f.write(f"s3://{BUCKET}/{src}\ts3://{BUCKET}/{dst}\n")
    print(f"skip (present, same size): {skipped:,} | to copy here: {len(todo):,} | "
          f"left to `aws s3 cp` (multipart sources): {len(multipart)} -> {mp_file} | conflicts: {len(conflicts)}", flush=True)
    for c in conflicts[:10]:
        print(f"CONFLICT {c[0]} exists with {c[1]} bytes, plan says {c[2]}", flush=True)
    if conflicts:
        sys.exit(1)
    if a.dry_run:
        return

    lock, done, failed, t0 = threading.Lock(), [0], [0], time.time()
    log = open(LOG, "a")

    def one(pair):
        src, dst = pair
        try:
            s3.copy_object(Bucket=BUCKET, Key=dst, CopySource={"Bucket": BUCKET, "Key": src})
            err = None
        except Exception as e:  # noqa: BLE001 — every failure is logged and counted
            err = repr(e)[:300]
        with lock:
            done[0] += 1
            if err:
                failed[0] += 1
                log.write(f"FAILED s3://{BUCKET}/{src} s3://{BUCKET}/{dst} {err}\n"); log.flush()
            if done[0] % 5000 == 0:
                dt = time.time() - t0
                print(f"{done[0]:,}/{len(todo):,} in {dt:,.0f} s ({done[0] / dt:,.0f}/s), failed {failed[0]}", flush=True)

    with ThreadPoolExecutor(a.workers) as ex:
        for _ in as_completed([ex.submit(one, p) for p in todo]):
            pass
    log.close()
    print(f"done: {done[0]:,} copies in {time.time() - t0:,.0f} s, failed {failed[0]}", flush=True)
    sys.exit(1 if failed[0] else 0)


if __name__ == "__main__":
    main()
