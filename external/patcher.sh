#! /bin/sh

readonly src="${1}"
readonly out="${2}"
readonly patches="${@}"

cp -r "${src}" "${out}"
chmod -R +w "${out}"
for p in $patches; do
    echo "Applying patch ${p}";
    patch -d "${out}" -p1 < "${p}";
done
