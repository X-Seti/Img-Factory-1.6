#!/usr/bin/env bash
#this belongs in tools/nightly_push.sh - Version: 1
# X-Seti - November 09 2026 - IMG Factory 1.6 - Nightly push of every repo

# Commits and pushes every repo under GIT_ROOT that has changes.
# Uses your normal git login (credential helper / SSH key); no tokens here.
#
# Run:      tools/nightly_push.sh            (push)
#           tools/nightly_push.sh --dry-run  (list only)
# Nightly:  crontab -e  ->  30 2 * * * $HOME/Documents/GitHub/Img-Factory-1.6/tools/nightly_push.sh >> $HOME/.cache/nightly_push.log 2>&1

##Methods list -
# push_repo

ROOT="${GIT_ROOT:-$HOME/Documents/GitHub}"
SKIP=("Img-Factory-plugins")        # GTA V / RDR2 code: never pushed by this script
DRY=0
[ "$1" = "--dry-run" ] && DRY=1
STAMP="$(date +%F)"

push_repo() { #vers 1
    local d="$1" name branch
    name="$(basename "$d")"
    branch="$(git -C "$d" symbolic-ref --short -q HEAD)" || { echo "skip  $name (detached HEAD)"; return; }
    git -C "$d" rev-parse --abbrev-ref '@{u}' >/dev/null 2>&1 || { echo "skip  $name (no upstream)"; return; }
    local dirty ahead
    dirty="$(git -C "$d" status --porcelain)"
    git -C "$d" fetch -q || { echo "FAIL  $name (fetch)"; return; }
    if [ $DRY -eq 1 ]; then
        ahead="$(git -C "$d" rev-list --count '@{u}..HEAD')"
        [ -n "$dirty" ] || [ "$ahead" != "0" ] && echo "todo  $name ($branch)"
        return
    fi
    if [ -n "$dirty" ]; then
        git -C "$d" add -A && git -C "$d" commit -q -m "Nightly update $STAMP" || { echo "FAIL  $name (commit)"; return; }
    fi
    if ! git -C "$d" pull -q --rebase --autostash; then      # remote changed too
        git -C "$d" rebase --abort 2>/dev/null
        echo "FAIL  $name (conflict - fix by hand)"
        return
    fi
    ahead="$(git -C "$d" rev-list --count '@{u}..HEAD')"
    if [ "$ahead" = "0" ]; then
        return
    fi
    git -C "$d" push -q && echo "push  $name ($ahead commit(s))" || echo "FAIL  $name (push)"
}

for d in "$ROOT"/*/; do
    d="${d%/}"
    [ -d "$d/.git" ] || continue
    for s in "${SKIP[@]}"; do [ "$(basename "$d")" = "$s" ] && continue 2; done
    push_repo "$d"
done
