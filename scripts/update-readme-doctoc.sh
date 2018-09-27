#!/usr/bin/env bash

exec "$(dirname ${BASH_SOURCE[0]})/doctoc/wrapper.sh" --github --title "**Table of Contents**" "$(dirname ${BASH_SOURCE[0]})/../README.md"
