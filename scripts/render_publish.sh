#!/usr/bin/env bash
# Detached render of a publishing notebook from the laptop, with the bookkeeping a later session needs to
# tell finished from stuck without attaching: a timestamped log, a `latest.txt` pointer to it, and a
# `.done` marker holding the exit code, all under _output/logs/. Publishing flags are ordinary env vars
# (`VAR=1` arguments or the caller's environment); the notebook's own gates decide what they do.
#
#   scripts/render_publish.sh <notebook.qmd> [VAR=value ...]     start a render (returns at once)
#   scripts/render_publish.sh status <notebook.qmd>              exit code if done, else the last log lines
#
#   NATIVE_LEGACY_OK=1 PUBLISH_MERGED_COG=1 scripts/render_publish.sh publish_native.qmd
#   scripts/render_publish.sh release_marine-atlas.qmd RELEASE_NO_S3=1 RELEASE_S3_TABLES=1
#
# Quarto in a sandboxed shell needs a TMPDIR it may write (.tmp/ here); the version in the log name is the
# notebook's (`ver` in libs/paths.R, VER= overrides). Logs: _output/logs/<stem>_<ver>_<YYYYmmdd_HHMM>.log.
set -euo pipefail
cd "$(dirname "$0")/.."

mode=render
if [ "${1:-}" = "status" ]; then mode=status; shift; fi
nb="${1:?usage: render_publish.sh [status] <notebook.qmd> [VAR=value ...]}"; shift || true
[ -f "$nb" ] || { echo "no such notebook: $nb" >&2; exit 2; }
stem="${nb%.qmd}"
ver="${VER:-$(sed -n 's/^ver *<- *"\([^"]*\)".*/\1/p' libs/paths.R | head -1)}"
logs=_output/logs; mkdir -p "$logs" .tmp
latest="$logs/${stem}_${ver}_latest.txt"; done_f="$logs/${stem}_${ver}.done"

if [ "$mode" = status ]; then
  [ -f "$latest" ] || { echo "no run recorded for $nb ($ver)"; exit 3; }
  log="$(cat "$latest")"; echo "log: $log"
  if [ -f "$done_f" ]; then echo "done: $(cat "$done_f")"; else echo "done: (running)"; fi
  sed 's/\x1b\[[0-9;]*m//g' "$log" | grep -E "INFO|WARN|rror|Output created" | tail -12
  exit 0
fi

for kv in "$@"; do                       # VAR=value arguments become the render's environment
  case "$kv" in *=*) export "$kv" ;; *) echo "not VAR=value: $kv" >&2; exit 2 ;; esac
done
log="$logs/${stem}_${ver}_$(date +%Y%m%d_%H%M).log"
echo "$log" > "$latest"; rm -f "$done_f"
TMPDIR="$PWD/.tmp" nohup sh -c "quarto render '$nb' > '$log' 2>&1; echo \"exit \$?\" > '$done_f'" > /dev/null 2>&1 &
echo "started $nb ($ver) -> $log; marker $done_f"
