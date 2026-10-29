#!/usr/bin/env python3
"""Recover a release's content-hash checkpoints from git history.

`data/manifests/<target>.json` holds ONE version's checkpoint at a time (the version `libs/paths.R`
points at), so when `ver` moves on (v8 -> v9) the previous version's checkpoints survive only in git.
A version bootstrapped from an older release (`bootstrap_version.qmd`, `BOOTSTRAP_CLONE_MERGE=1`)
needs those checkpoints to prove what it cloned is what that release checkpointed.

For each target this walks the manifest's history to the LAST commit whose manifest carried
`"ver": "<ver>"` and records its `content_hash` (+ `er_hash` where present), the commit and its date,
into `data/manifests/checkpoints_<ver>.json`. Re-running is idempotent; the file is committed.

    python3 scripts/checkpoints_from_git.py v8
"""
import json, subprocess, sys, os

TARGETS = ["bootstrap_version", "merge_models", "merge_taxon", "score_zones",
           "score_cell_metrics", "score_zone_metrics", "build_registry"]

def git(*a):
    return subprocess.run(["git", *a], capture_output=True, text=True, check=True).stdout

def main(ver):
    repo = git("rev-parse", "--show-toplevel").strip()
    os.chdir(repo)
    out = {"ver": ver, "targets": {}}
    for t in TARGETS:
        path = f"data/manifests/{t}.json"
        commits = git("log", "--format=%H %cs", "--", path).splitlines()
        found = None
        for line in commits:                       # newest first
            sha, date = line.split()
            try:
                d = json.loads(git("show", f"{sha}:{path}"))
            except subprocess.CalledProcessError:
                continue
            if d.get("ver") == ver:
                found = {"content_hash": d["content_hash"], "commit": sha, "date": date}
                for k in ("er_hash", "n_cells", "n_taxa", "composite_cells", "n_zones", "n_zone_cells"):
                    if k in d and d[k] is not None:
                        found[k] = d[k]
                break
        if found:
            out["targets"][t] = found
            print(f"{t:22s} {found['content_hash']}  {found['commit'][:8]} {found['date']}")
        else:
            print(f"{t:22s} (no commit with ver = {ver})")
    dst = f"data/manifests/checkpoints_{ver}.json"
    with open(dst, "w") as f:
        json.dump(out, f, indent=2); f.write("\n")
    print("wrote", dst)

if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "v8")
