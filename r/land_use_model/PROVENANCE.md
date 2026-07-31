# Provenance

These scripts are a vendored copy — not a fork — of the parallelized land-use model pipeline from
the private emLab repo `emlab-ucsb/land-based-solutions`.

| | |
|---|---|
| Upstream repo | `emlab-ucsb/land-based-solutions` (private) |
| Upstream path | `r/robert_test/parallelization/` |
| Upstream commit | `e207c2bec24743866004b9f4eed4fe175601bbb3` (default branch `main`, 2026-07-31) |
| Copied on | 2026-07-31 |

`../directories.R` was copied from upstream `r/directories.R`, which every script in the pipeline
sources for the Nextcloud data paths.

Not copied: `logs/` (old upstream pipeline run logs).

## Local modifications

1. Directory renamed `r/robert_test/parallelization/` → `r/land_use_model/`, and the corresponding
   `here::here("r/robert_test/parallelization/...")` calls in all 17 scripts rewritten to
   `here::here("r/land_use_model/...")`. No other changes on import.

_Record further modifications below as the price-shock scenarios are wired in._

## Checking upstream for changes

This is a deliberate copy, not a fork or subtree — see "Why a copy" below. There is no git link to
upstream, so checking for upstream changes means diffing against a fresh download.

Run from the repo root (requires `gh` authenticated with access to the private repo; `sed -i ''` is
the BSD/macOS form). This is read-only — it downloads to a temp directory and touches nothing under
`r/`:

```sh
UP=repos/emlab-ucsb/land-based-solutions/contents/r/robert_test/parallelization
TMP=$(mktemp -d)
mkdir -p "$TMP/pipeline"
gh api "$UP" --jq '.[] | select(.type=="file") | .name' | while read -r f; do
  gh api "$UP/$f" -H "Accept: application/vnd.github.raw" > "$TMP/pipeline/$f"
done
gh api repos/emlab-ucsb/land-based-solutions/contents/r/directories.R \
  -H "Accept: application/vnd.github.raw" > "$TMP/directories.R"

# apply modification 1 so the rename doesn't show up as a spurious diff
sed -i '' 's|r/robert_test/parallelization|r/land_use_model|g' "$TMP/pipeline"/*.R

echo "=== pipeline ===";       diff -ru -x PROVENANCE.md r/land_use_model "$TMP/pipeline"
echo "=== directories.R ==="; diff -u r/directories.R "$TMP/directories.R"
gh api repos/emlab-ucsb/land-based-solutions/commits --jq '.[0] | "upstream HEAD: \(.sha)"'
rm -rf "$TMP"
```

Empty diffs mean upstream is unchanged relative to this copy. If something did change, decide
file by file whether it matters here, copy in only what you want, and update the commit pin above.
Do not bulk-overwrite — that would silently revert the local modifications listed above.

## Why a copy

Considered and rejected:

- **`git subtree` without `--squash`** — makes all ~1,424 upstream commits ancestors of this repo's
  history, for the entire upstream repo, not just these files. Only ~84 of them touch this pipeline,
  and the historical commits carry the pre-rename paths, so `git log -- r/land_use_model/` would show
  almost nothing while `git log` showed everything.
- **`git subtree --squash`** — clean history, but adds little over a plain copy, and the underlying
  `git fetch` still pulls upstream's full ~34 MB pack into `.git`.
- **`git filter-repo` path-filtered mirror** — the only way to get history for just these files, but
  the rewritten SHAs correspond to nothing upstream, and it means maintaining a rewrite pipeline.

The deciding factor: this copy diverges from upstream immediately and permanently (carbon price
forced to 0, scenario price-vector input, possibly a reduced crop set), and upstream is an active
research repo rather than a versioned dependency. Upstream changes will essentially never be
absorbed wholesale, so the useful question is "did upstream change something that matters here?" —
which is a diff, not a merge.
