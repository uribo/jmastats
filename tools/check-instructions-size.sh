#!/bin/sh
# Guard the size of the agent instruction chain that Codex loads.
#
# Codex walks from the git root down to the working directory and, in each
# directory, takes AGENTS.override.md, then AGENTS.md, then any name in
# project_doc_fallback_filenames. The concatenated project doc is cut at
# project_doc_max_bytes (32 KiB by default) WITHOUT a warning: content past
# the limit simply never reaches the model. Measured with
# `codex debug prompt-input` on 2026-09-17: a 44,961-byte file was delivered
# as exactly 32,768 bytes, truncated mid-sentence.
#
# Claude Code has no such limit (it loads CLAUDE.md, and the @AGENTS.md import
# with it, up to 4 MiB), so an oversized file fails in only one of the two
# tools — silently. This script makes that failure loud.
#
# Usage:
#   sh tools/check-instructions-size.sh            # working tree
#   sh tools/check-instructions-size.sh --staged   # staged content (hooks)
#
# Exit codes: 0 ok (a warning still exits 0), 1 over the hard limit.

HARD_LIMIT=32768 # project_doc_max_bytes default
WARN_LIMIT=30720 # 94% of the hard limit

mode=worktree
[ "$1" = "--staged" ] && mode=staged

status=0

# Every AGENTS.md from the repository root down. Nested files only load when
# Codex runs in that subdirectory, so they are reported separately rather than
# summed into one total.
files=$(git ls-files '*AGENTS.md' 'AGENTS.md' 2>/dev/null | sort -u)
[ -n "$files" ] || files=AGENTS.md

for f in $files; do
  if [ "$mode" = staged ]; then
    bytes=$(git show ":$f" 2>/dev/null | wc -c | tr -d ' ')
    [ "$bytes" = "0" ] && continue # not staged in this commit
  else
    [ -f "$f" ] || continue
    bytes=$(wc -c <"$f" | tr -d ' ')
  fi

  if [ "$bytes" -gt "$HARD_LIMIT" ]; then
    over=$((bytes - HARD_LIMIT))
    echo "ERROR: $f is ${bytes} bytes, ${over} bytes over Codex's ${HARD_LIMIT}-byte limit." >&2
    echo "       Codex will drop the last ${over} bytes without a warning." >&2
    echo "       Move detail into docs/ and leave a pointer line (see AGENTS.md, 指示ファイルの構成)." >&2
    status=1
  elif [ "$bytes" -gt "$WARN_LIMIT" ]; then
    echo "WARN: $f is ${bytes} bytes; Codex truncates at ${HARD_LIMIT}. Room left: $((HARD_LIMIT - bytes)) bytes." >&2
  fi
done

exit $status
