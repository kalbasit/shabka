#! /usr/bin/env bash

readonly here="$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)"

find "${here}" -mindepth 2 -name update.sh -exec {} \;
