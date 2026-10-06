#!/usr/bin/env bash
# the msens changes to OBIS's pipeline (iobis/mpaeu_sdm) and its package (iobis/mpaeu_msdm, obissdm), kept
# here as patch series so they are recoverable from this repo. the working copies are local branches
# `msens-patches` of the two clones; nothing is pushed to iobis and there is no public fork.
#   scripts/og/patches.sh export    # clones -> scripts/og/patches/{repo}/*.patch + base.tsv (run after every commit there)
#   scripts/og/patches.sh apply     # base.tsv + patches -> branch msens-patches (clones the repo if it is missing)
#   scripts/og/patches.sh check     # apply into throwaway clones and compare the tree hashes with base.tsv
# base.tsv: repo, upstream url, upstream commit the series starts from, head commit, head TREE hash. the tree
# hash is what `apply`/`check` assert: commit ids differ between machines (committer, date), trees do not.
set -euo pipefail

DIR_OG=$(cd "$(dirname "$0")" && pwd)
DIR_PATCH=$DIR_OG/patches
DIR_CLONES=${OG_CLONES:-$HOME/Github/iobis}
BRANCH=msens-patches
REPOS="mpaeu_sdm mpaeu_msdm"

do_export() {
  local tsv=$DIR_PATCH/base.tsv
  mkdir -p "$DIR_PATCH"
  printf 'repo\turl\tbase\thead\ttree\n' > "$tsv"
  for r in $REPOS; do
    local c=$DIR_CLONES/$r
    [ -z "$(git -C "$c" status --porcelain)" ] || { echo "ERROR: $c has uncommitted changes" >&2; exit 1; }
    local up; up=$(git -C "$c" symbolic-ref -q --short refs/remotes/origin/HEAD || echo origin/main)
    local base; base=$(git -C "$c" merge-base "$BRANCH" "$up")
    rm -rf "$DIR_PATCH/$r"; mkdir -p "$DIR_PATCH/$r"
    git -C "$c" format-patch --quiet --no-signature --zero-commit -o "$DIR_PATCH/$r" "$base..$BRANCH"
    printf '%s\t%s\t%s\t%s\t%s\n' "$r" "$(git -C "$c" remote get-url origin)" "$base" \
      "$(git -C "$c" rev-parse "$BRANCH")" "$(git -C "$c" rev-parse "$BRANCH^{tree}")" >> "$tsv"
    echo "$r: $(ls "$DIR_PATCH/$r" | wc -l | tr -d ' ') patch(es) on $base"
  done
}

# apply_one <repo> <url> <base> <tree> <clone dir>
apply_one() {
  local r=$1 url=$2 base=$3 tree=$4 c=$5
  [ -d "$c/.git" ] || git clone --quiet "$url" "$c"
  if git -C "$c" rev-parse -q --verify "refs/heads/$BRANCH" > /dev/null; then
    echo "$r: branch $BRANCH exists, not re-applied"
  else
    git -C "$c" cat-file -e "$base^{commit}" 2> /dev/null || git -C "$c" fetch --quiet origin
    git -C "$c" checkout --quiet -b "$BRANCH" "$base"
    git -C "$c" -c user.name="${GIT_AUTHOR_NAME:-msens}" -c user.email="${GIT_AUTHOR_EMAIL:-msens@localhost}" \
      am --quiet "$DIR_PATCH/$r"/*.patch
  fi
  local got; got=$(git -C "$c" rev-parse "$BRANCH^{tree}")
  [ "$got" = "$tree" ] || { echo "ERROR: $r tree $got, expected $tree" >&2; exit 1; }
  echo "$r: $BRANCH tree $got ok"
}

do_apply() { # do_apply <dir of clones>
  mkdir -p "$1"
  tail -n +2 "$DIR_PATCH/base.tsv" | while IFS=$'\t' read -r r url base head tree; do
    apply_one "$r" "$url" "$base" "$tree" "$1/$r"
  done
}

case "${1:-}" in
  export) do_export ;;
  apply)  do_apply "$DIR_CLONES" ;;
  check)  # throwaway clones of upstream history only (--single-branch of the base's branch has no msens-patches)
    tmp=$(mktemp -d "${TMPDIR:-/tmp}/og_patches.XXXXXX"); trap 'rm -rf "$tmp"' EXIT
    for r in $REPOS; do
      up=$(git -C "$DIR_CLONES/$r" symbolic-ref -q --short refs/remotes/origin/HEAD || echo origin/main)
      git clone --quiet --no-local --single-branch --branch "${up#origin/}" "$(git -C "$DIR_CLONES/$r" remote get-url origin)" "$tmp/$r" 2> /dev/null ||
        { git init --quiet "$tmp/$r"; git -C "$tmp/$r" fetch --quiet "$DIR_CLONES/$r" "refs/remotes/$up:refs/heads/upstream"; }
    done
    do_apply "$tmp" ;;
  *) echo "usage: $0 export|apply|check" >&2; exit 2 ;;
esac
