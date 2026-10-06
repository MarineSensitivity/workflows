# msens patches to the OBIS SDM pipeline

Patch series for the two upstream repositories the `og` runs use, written by `scripts/og/patches.sh export`
from the local branches `msens-patches`. Nothing here is pushed to `iobis`, and there is no public fork.

| folder | upstream | licence |
|---|---|---|
| `mpaeu_sdm/` | <https://github.com/iobis/mpaeu_sdm> (the pipeline) | CC-BY-IGO-4.0, Intergovernmental Oceanographic Commission of UNESCO |
| `mpaeu_msdm/` | <https://github.com/iobis/mpaeu_msdm> (the `obissdm` package) | MIT, obissdm authors |

`base.tsv` records, per repository, the upstream commit the series starts from and the tree hash of the
patched head. `scripts/og/patches.sh apply` rebuilds the branch on a machine that lacks it and stops if the
tree differs; `scripts/og/patches.sh check` does the same in throwaway clones. Re-export after every commit
on `msens-patches`, and commit the result with the run it belongs to: a run's `manifest.json` names the
commit it was fitted with.

All options are off by default, so an unset environment runs the pipeline as OBIS wrote it, apart from the
current-only scenarios, the per-species seed and the saved fold predictions.
