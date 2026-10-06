#!/usr/bin/env bash
# render the range-constraint scorecard for one model run. The body is _obis_range_constraint.qmd;
# each model has a thin wrapper .qmd (its own title, `params: model`, and its own _files directory,
# so two models' figures never overwrite each other).
#   scripts/og/render_scorecard.sh            # mpaeu = OBIS's published models -> _output/obis_range_constraint.html
#   scripts/og/render_scorecard.sh og         # our run -> _output/obis_range_constraint_og.html
#   scripts/mm_run.sh og_card scripts/og/render_scorecard.sh og     # under tmux on the mini
# env REDO_OG_CELLS=1 rebuilds the model's cached cell table; OG_OUT = root of our runs (default ~/_big/sdm/og)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
model=${1:-mpaeu}
[[ $model =~ ^[a-z0-9]+$ ]] || { echo "model must match ^[a-z0-9]+$" >&2; exit 2; }
mkdir -p .tmp; export TMPDIR=$PWD/.tmp
qmd=obis_range_constraint.qmd
if [ "$model" != mpaeu ]; then
  qmd=obis_range_constraint_${model}.qmd
  # a wrapper that does not exist yet is written from the published one (commit it with its output)
  [ -f "$qmd" ] || sed -e "s/^  model: mpaeu /  model: $model /" \
    -e "s/scorecard on the published OBIS models/scorecard on our OBIS-method run \`$model\`/" obis_range_constraint.qmd > "$qmd"
  grep -q "^  model: $model " "$qmd" || { echo "$qmd does not set params model: $model" >&2; exit 1; }
fi
quarto render "$qmd"
