#!/usr/bin/env sh

echo $(TZ=UTC git log -1 --pretty=%ad-g%h --date=format-local:"%Y%m%d_%H%M%S" -- "$@")$(test -z "$(git status --short -- "$@")" || echo _DIRTY)
