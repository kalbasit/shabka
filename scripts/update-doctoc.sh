#!/usr/bin/env bash

readonly here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "${here}/.."

find . -name README.md -exec ./libexec/doctoc/wrapper.sh --github --title "**Table of Contents**" {} \;
find doc -name '*.md' -exec ./libexec/doctoc/wrapper.sh --github --title "**Table of Contents**" {} \;
