#! /usr/bin/env bash

rootdir=$(realpath "$(dirname "$0")/../")
langs=( 'pl' 'en_pt' )

for lang in ${langs[@]}; do
    file="${rootdir}/translations/${lang}.translation"
    find "${rootdir/src/}" -name \*.elm -print0 \
    | xargs -r0 sed -n -E 's/^.*translate (.*) "(.*)".*$/\2/p' \
    | sort -u \
    | while read key; do
        current_value=$(awk -F '|' -v key="$key" '{ gsub(/[ \t]+$/,"",$1); gsub(/^[ \t]+/,"",$2) } $1==key {print $2}' $file)
        printf "%-80s | %s\n" "$key" "${current_value:-TRANSLATION}" >> "${file}.new"
    done
    mv "${file}.new" "${file}"
done
