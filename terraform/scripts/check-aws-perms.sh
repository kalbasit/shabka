#! /usr/bin/env bash

set -euo pipefail

tmpFile="$(mktemp)"
function cleanup() {
	rm -rf "${tmpFile}"
}
trap cleanup EXIT INT QUIT TERM

echo -n 'hello' > "${tmpFile}"

# TODO: the flag --no-verify-ssl is being used because the aws command is failing with the message
#  [SSL: CERTIFICATE_VERIFY_FAILED] certificate verify failed (_ssl.c:726)
aws --no-verify-ssl s3 cp "${tmpFile}" "s3://nasreddine-infra/terraform/$(basename "${tmpFile}")" &>/dev/null
aws --no-verify-ssl s3 cp "s3://nasreddine-infra/terraform/$(basename "${tmpFile}")" "${tmpFile}" &>/dev/null
aws --no-verify-ssl s3 rm "s3://nasreddine-infra/terraform/$(basename "${tmpFile}")" &>/dev/null

if [[ "$(cat "${tmpFile}")" == "hello" ]]; then
	echo 1
	exit 0
fi

exit 1
