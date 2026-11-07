#!/usr/bin/env bash
# Kill a render running in ONE tmux window of ours, and nothing else on the machine: the pane's shell and every
# process descended from it (bash -> quarto -> deno -> R ... ), found by walking child PIDs from the pane PID.
# Never `pkill -f "R --no-echo"` / `pkill -f rmd.R` on the Mac mini: that pattern matches every Rscript and every
# Quarto knitr child of every project, and on 2026-10-30 it killed a CalCOFI release render in another session.
#
#   scripts/tmux_kill_window.sh msens:v11-full        # TERM the tree, wait 10 s, KILL what is left, close the window
#   scripts/tmux_kill_window.sh msens:v11-full --dry   # list the PIDs it would kill
set -uo pipefail
win="${1:?usage: $0 <session:window> [--dry]}"; dry="${2:-}"
pane_pid=$(tmux list-panes -t "$win" -F '#{pane_pid}' 2>/dev/null | head -1)
[ -n "$pane_pid" ] || { echo "no such tmux window: $win" >&2; exit 1; }
descendants() { local p; for p in $(pgrep -P "$1"); do echo "$p"; descendants "$p"; done; }
pids="$(descendants "$pane_pid")"
echo "window $win pane $pane_pid -> $(echo $pids | wc -w | tr -d ' ') descendant process(es)"
ps -o pid,etime,rss,command -p $(echo $pane_pid $pids | tr ' ' ',') 2>/dev/null | cut -c1-110
[ "$dry" = "--dry" ] && exit 0
[ -n "$pids" ] && kill $pids 2>/dev/null
sleep 10
left=$(for p in $pids; do kill -0 "$p" 2>/dev/null && echo "$p"; done)
[ -n "$left" ] && { echo "KILL: $left"; kill -9 $left 2>/dev/null; }
tmux kill-window -t "$win" 2>/dev/null
echo "done; still alive from the tree: $(for p in $pids; do kill -0 "$p" 2>/dev/null && echo -n "$p "; done)"
