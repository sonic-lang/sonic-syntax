#!/usr/bin/env bash

set -euo pipefail

function main() {
  local -r repo_url="https://github.com/${GITHUB_REPOSITORY}"
  local -r version=${GITHUB_REF#refs\/tags\/v}

  local -r release_badge="[![release](${repo_url}/workflows/${GITHUB_WORKFLOW}/badge.svg)](${repo_url}/actions/runs/${GITHUB_RUN_ID})"
  local release_message
  # shellcheck disable=SC2016
  release_message="$(git tag -l --format='$(contents)' "v${version}")"

  local package
  package="$(sed -e 's/^name:\s*\(.*\)$/\1/;t;d' ./*.cabal)"
  local package_badge="[![Hackage](https://img.shields.io/badge/hackage-v${version}-blue)](https://hackage.haskell.org/package/${package}-${version})"

  # shellcheck disable=SC2155
  local body=$(cat << EOS
${release_badge} ${package_badge}
${release_message}
EOS
)

  # https://github.community/t5/GitHub-Actions/set-output-Truncates-Multiline-Strings/td-p/37870
  body="${body//'%'/'%25'}"
  body="${body//$'\n'/'%0A'}"
  body="${body//$'\r'/'%0D'}"

  echo "::set-output name=body::${body}"
  echo "::set-output name=name::v${version}"
}

main
