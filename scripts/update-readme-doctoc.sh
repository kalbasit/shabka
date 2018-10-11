#!/usr/bin/env bash

readonly here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "${here}/.."

find . -name README.md -exec ./scripts/doctoc/wrapper.sh --github --title "**Table of Contents**" {} \;
