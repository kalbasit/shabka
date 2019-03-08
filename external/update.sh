#! /usr/bin/env bash

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)"

find "${here}" -name update.sh -exec {} \;
