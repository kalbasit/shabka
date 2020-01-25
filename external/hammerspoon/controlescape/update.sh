#! /usr/bin/env bash

set -euo pipefail

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)"
readonly shabka_path="/run/current-system/shabka"

exec "${shabka_path}/scripts/update-external.sh" jasonrudolph ControlEscape.spoon master "${here}/version.json"
