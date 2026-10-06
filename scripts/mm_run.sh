#!/usr/bin/env bash
# run a long command on the mac mini under tmux, with a log and an exit-code file, so a later
# session can tell finished from stuck without attaching (the mini's twin of srv_render.sh).
#   scripts/mm_run.sh <name> <command...>      # from the mini itself
#   ssh macmini '~/Github/MarineSensitivity/workflows/scripts/mm_run.sh <name> <command...>'
# writes ~/logs/<name>.log and ~/logs/<name>.exit (absent while the run is alive).
# check with: tail -n 20 ~/logs/<name>.log ; cat ~/logs/<name>.exit   (never cat a whole log)
set -euo pipefail

[ $# -ge 2 ] || { echo "usage: $0 <name> <command...>" >&2; exit 2; }
name=$1; shift
dir_log=${MM_LOG_DIR:-$HOME/logs}
mkdir -p "$dir_log"

export PATH=/opt/homebrew/bin:/usr/local/bin:$HOME/.local/bin:$PATH
if tmux has-session -t "=$name" 2>/dev/null; then
  echo "tmux session '$name' is still running; refusing to start a second one" >&2; exit 1
fi

# keep the previous log (a rerun must not erase the record of the run before it)
[ -f "$dir_log/$name.log" ] && mv "$dir_log/$name.log" "$dir_log/$name.$(date -r "$dir_log/$name.log" +%Y%m%dT%H%M%S).log"
rm -f "$dir_log/$name.exit"

cmd="$*"
tmux new-session -d -s "$name" -c "$PWD" \
  "export PATH='$PATH'; { echo \"# \$(date '+%F %T') \$(hostname -s) \$PWD\"; echo '# $cmd'; $cmd; } > '$dir_log/$name.log' 2>&1; echo \$? > '$dir_log/$name.exit'"
echo "started tmux '$name' -> $dir_log/$name.log"
