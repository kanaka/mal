#!/bin/bash

set -e

die() { echo "${*}"; exit 1; }
usage() {
    [ "${*}" ] && echo >&2 -e "${*}\n"
    echo "Usage: $0 REPO BRANCH [VAR=VAL]...

    Examples:

      Trigger build/test in self-hosted mode:
        $0 REPO BRANCH DO_SELF_HOST=1

      Trigger build/test with stop on soft failures:
        $0 REPO BRANCH DO_HARD=1

      Trigger build/test using regress mode on stepA:
        $0 REPO BRANCH REGRESS=1 STEP=stepA

      Trigger build/test using regress mode on all steps:
        $0 REPO BRANCH REGRESS=1
    " | sed 's/^    //' >&2

    exit 2
}

REPO="${1}"; shift || usage "REPO required"
BRANCH="${1}"; shift || usage "BRANCH required"
VARS="${*}"

repo="${REPO/\//%2F}"
vars=""
[ "${VARS}" ] && vars="\"${VARS// /\", \"}\""

body="{
  \"request\": {
    \"message\": \"Manual build. Settings: ${VARS}\",
    \"branch\":\"${BRANCH}\",
    \"config\": {
      \"env\": {
        \"global\": [${vars}]
      }
    }
  }
}"

token="$(travis token --org --no-interactive)"

curl -X POST \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -H "Travis-API-Version: 3" \
  -H "Authorization: token ${token}" \
  -d "$body" \
  "https://api.travis-ci.org/repo/${repo}/requests"
