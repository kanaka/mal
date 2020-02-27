#!/bin/bash

# Reference: https://docs.travis-ci.com/user/triggering-builds/

set -e

die() { echo "${*}"; exit 1; }
usage() {
  [ "${*}" ] && echo >&2 -e "${*}\n"
  echo "Usage: $0 REPO BRANCH [VAR=VAL]...

  Authorization:

    If you have the travis program installed then it will be called
    to get an API token (you need to have done 'travis login --org'
    in the past). Alternately you can explicity pass a token using
    the TRAVIS_TOKEN environment variable. You can see your API
    token at https://travis-ci.org/account/preferences.

  Travis .org vs .com:

    By default 'api.travis-ci.org' is used for API calls. This can
    be overridden by setting TRAVIS_HOST="api.travis-ci.com"

  Examples:

    Trigger build/test in self-hosted mode:
      $0 REPO BRANCH DO_SELF_HOST=1

    Trigger build/test with stop on soft failures:
      $0 REPO BRANCH DO_HARD=1

    Trigger build/test using regress mode on stepA:
      $0 REPO BRANCH REGRESS=1 STEP=stepA

    Trigger build/test using regress mode on all steps:
      $0 REPO BRANCH REGRESS=1
  " | sed 's/^  //' >&2

  exit 2
}

TRAVIS_TOKEN="${TRAVIS_TOKEN:-}" # default to travis program
TRAVIS_HOST="${TRAVIS_HOST:-api.travis-ci.org}"

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

if [ -z "${TRAVIS_TOKEN}" ]; then
    which travis >/dev/null \
        || die "TRAVIS_TOKEN not set and travis command not found"
    TRAVIS_TOKEN="$(travis token --org --no-interactive)"
fi

curl -X POST \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -H "Travis-API-Version: 3" \
  -H "Authorization: token ${TRAVIS_TOKEN}" \
  -d "$body" \
  "https://${TRAVIS_HOST}/repo/${repo}/requests"
