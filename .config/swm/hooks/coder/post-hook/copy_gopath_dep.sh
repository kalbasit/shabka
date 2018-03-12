#!/bin/sh

BASE_STORY_GOPATH="${1}"
STORY_GOPATH="${2}"

# run only if the GOPATH is different and the dep folder does not exist in the story GOPATH
if [[ "${BASE_STORY_GOPATH}" != "${STORY_GOPATH}" ]] && [[ ! -d "${STORY_GOPATH}/pkg/dep" ]]; then
	mkdir -p "${STORY_GOPATH}/pkg"
	cp -R "${BASE_STORY_GOPATH}/pkg/dep" "${STORY_GOPATH}/pkg/dep"
fi
