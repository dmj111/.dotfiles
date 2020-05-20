#!/usr/bin/env bash

# Cleanup stash's left behind by git-trash

# - Restrict to stash's with GITTRASH in the message
# - Keep the most recent 5 (just in case)
# - Reverse the order ... if we delete git stash(1) first, then
#   the rest are renumbered.
git stash list --grep=GITTRASH: \
    | cut -f 1 -d : \
    | tail -n +6 \
    | tac \
    | xargs -n 1 git stash drop
