#!/usr/bin/env bash
# One-screen status of the mini's msens pipeline for the hourly heartbeat (committed so the report is the same
# every hour): timestamp, each tmux window of session `msens` with what it is running, the attribution loop's
# current variant / stage / last log line, the snapshots in hand, disk and memory, R processes of ours.
cd "$(dirname "$0")/.."
echo "== msens heartbeat $(date '+%Y-%m-%d %H:%M %Z') on $(hostname -s)"
echo "-- tmux (session msens): $(tmux list-windows -t msens -F '#W' 2>/dev/null | tr '\n' ' ')"
if [ -f _output/logs/attribution.log ]; then
  echo "-- attribution loop: $(grep -E '^==> variant' _output/logs/attribution.log | tail -1 | sed 's/==> //')"
  echo "   stage: $(grep -E '^==>' _output/logs/attribution.log | grep -v variant | tail -1 | sed 's/==> //')"
  grep -qE '^!!|EXIT=' _output/logs/attribution.log && echo "   end: $(grep -E '^!!|EXIT=|^==> attribution runs done' _output/logs/attribution.log | tail -1)"
fi
last_stage_log=$(ls -t _output/logs/*.log 2>/dev/null | grep -v "attribution\|heartbeat" | head -1)
[ -n "$last_stage_log" ] && echo "   last line ($(basename "$last_stage_log")): $(sed 's/\x1b\[[0-9;]*m//g' "$last_stage_log" | grep -E 'INFO|WARN|Error|Output created' | tail -1 | cut -c1-150)"
echo "-- snapshots: $(ls data/v11/attribution/ 2>/dev/null | sed 's/.parquet//' | tr '\n' ' ')"
echo "-- disk: $(df -h /Volumes/msens_big | tail -1 | awk '{print $4 " free of " $2}')  spill: $(du -sh ~/_big/msens/derived/v11/_tmp ~/_big/msens/derived/v11/marine-atlas/duckdb_tmp 2>/dev/null | awk '{s=s $1 " "} END {print s}')"
echo "-- R: $(ps -eo etime,pcpu,rss,command | grep -E '[/]R --no-echo' | awk '{printf "%s %s%%cpu %.1fGB; ", $1, $2, $3/1048576}')"
echo "-- STATUS last row: $(grep -v "^$" _output/logs/STATUS.md | tail -1 | cut -c1-220)"
