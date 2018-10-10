#!/usr/bin/env bash
#
#  vim:ft=sh:
#
#  Copyright (c) 2010-2018 Wael Nasreddine <wael.nasreddine@gmail.com>
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
	local result current_workspaces current_profile current_story dir elem

	# print out the list of currently active workspaces
	result=( $(@i3-msg_bin@ -t get_workspaces | @jq_bin@ -r '.[] | if .focused == false then .name else empty end') )

	# print out the list of available stories, but only if we are on the base story
	current_workspaces="$( @i3-msg_bin@ -t get_workspaces | @jq_bin@ -r '.[] | if .focused == true then .name else empty end' )"
	current_profile="$( echo "${current_workspaces}" | cut -d\@ -f1 )"
	current_story="$( echo "${current_workspaces}" | cut -d\@ -f2 )"
	if [[ "${current_story}" == "base" ]]; then
		for dir in $(find "/code/${current_profile}/stories" -mindepth 1 -maxdepth 1); do
			if [[ -d "${dir}" ]]; then
				elem="${current_profile}@$(basename "${dir}")"
				if ! containsElement "${elem}" "${result[@]}"; then
					result=("${result[@]}" "${elem}")
				fi
			fi
		done
	fi

	# print out the list of available profiles
	for dir in $(find "/code/" -mindepth 1 -maxdepth 1); do
		if [[ -d "${dir}" ]] && [[ -d "${dir}/base" ]]; then
			elem="$(basename "${dir}")@base"
			if ! containsElement "${elem}" "${result[@]}"; then
				result=("${result[@]}" "${elem}")
			fi
		fi
	done

	for elem in "${result[@]}"; do
		echo "${elem}"
	done
}
