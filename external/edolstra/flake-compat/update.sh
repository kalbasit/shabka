#! /usr/bin/env bash

echo "$0 is disabled because the latest does not work"
exit 0

set -euo pipefail

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)"
readonly shabka_path="$(cd "${here}"/../../../ && pwd)"

exec "${shabka_path}/scripts/update-external.sh" edolstra flake-compat master "${here}/version.json"
