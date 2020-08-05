#!/usr/bin/env bash
#
#  vim:ft=sh:
#
#  Copyright (c) 2010-2020 Wael Nasreddine <wael.nasreddine@gmail.com>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
#  USA.
#

containsElement () {
	local e match="$1"
	shift
	for e; do [[ "$e" == "$match" ]] && return 0; done
	return 1
}

function listWorkspaces() {
    local result current_workspace current_profile current_story dir elem file elem

	# print out the list of currently active workspaces
	result=( $(@i3-msg_bin@ -t get_workspaces | @jq_bin@ -r '.[] | if .focused == false then .name else empty end') )

    # compute the current profile and the current story
	current_workspace="$( @i3-msg_bin@ -t get_workspaces | @jq_bin@ -r '.[] | if .focused == true then .name else empty end' )"
    if echo "${current_workspace}" | grep -q '@'; then
        current_profile="$( echo "${current_workspace}" | cut -d\@ -f1 )"
        current_story="$( echo "${current_workspace}" | cut -d\@ -f2 )"
    else
        current_profile="${current_workspace}"
    fi

	# print out the list of available stories, but only if there's no active story
	if [[ -z "${current_story:-}" ]]; then
        for story in $(swm story list --name-only | grep "^${current_profile}/" | cut -d/ -f2-); do
            elem="${current_profile}@${story}"
            if ! containsElement "${elem}" "${result[@]}"; then
                result+=("${elem}")
            fi
		done
	fi

	# print out the list of available profiles
    for file in $(find "${HOME}/.zsh/profiles" -iregex '.*/[a-z]*\.zsh' | sort); do
        elem="$(basename "${file}" .zsh)"
        if ! containsElement "${elem}" "${result[@]}"; then
            result=("${result[@]}" "${elem}")
        fi
    done

	for elem in "${result[@]}"; do
		echo "${elem}"
	done
}
