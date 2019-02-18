#!/usr/bin/env bash
# vim: sw=2 ts=2 sts=0 noet

{ # prevent the script from executing partially downloaded

set -euo pipefail

readonly color_clear="\033[0m"
readonly color_red="\033[0;31m"
readonly color_green="\033[0;32m"

info() {
	>&2 echo -e "[SHABKA] ${color_green}${@}${color_clear}"
}

error() {
	>&2 echo -e "[SHABKA] ${color_red}${@}${color_clear}"
}

# Prompt for  sudo password & keep alive
# Taken from https://github.com/LnL7/nix-darwin/blob/2412c7f9f98377680418625a3aa7b685b2403107/bootstrap.sh#L77-L83
sudo_prompt(){
	echo "Please enter your password for sudo authentication"
	sudo -k
	sudo echo "sudo authenticaion successful!"
	while true ; do sudo -n true ; sleep 60 ; kill -0 "$$" || exit ; done 2>/dev/null &
}

if [[ "${#}" -ne 1 ]]; then
	echo "USAGE: $0 <hostname>"
	exit 1
fi

readonly hostname="${1}"
readonly here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly shabka_path="$( cd "${here}/../.." && pwd )"
readonly hostcf="${shabka_path}/hosts/${hostname}"
readonly workdir="$(mktemp -d)"
readonly xdg_config_nixpkgs="${HOME}/.config/nixpkgs"
trap "rm -rf ${workdir}" EXIT

sudo_prompt

if ! defaults read com.github.kalbasit.shabka bootstrap >/dev/null 2>&1; then
	# Wipe all (default) app icons from the Dock
	defaults write com.apple.dock persistent-apps -array

	info "Installing Xcode command line tools"
	if xcode-select --install; then
		echo "Software update menu has now opened, please follow the instructions to get it installed."
		echo "Once the installation is finished, please press Enter"
		read -rn1
	fi

	# download and install Homebrew if it's not installed already
	command -v brew 2>/dev/null || {
		info "Installing HomeBrew"
		/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	}

	# download and install Nix
	command -v nix 2>/dev/null || {
		mkdir -p "${xdg_config_nixpkgs}"

		info "Installing Nix"
		sh <(curl https://nixos.org/nix/install) --daemon </dev/null >/dev/stdout 2>&1
		if ! grep -q nix-daemon.sh ~/.profile 2>/dev/null; then
			cat <<-EOF >> ~/.profile
			if [[ -r "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]]; then
				source "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
			fi
			EOF
		fi
		set +u
			# include nix in the current shell so we can install stuff
			if [[ -r "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]]; then
				source "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
			else
				>&2 echo "ERR: unable to find the nix-daemon script at /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
				exit 1
			fi
		set -u

		# make sure nix is now available
		if ! nix-shell -p hello --run 'hello' &>/dev/null; then
			# The following condition addresses https://github.com/NixOS/nix/issues/2523
			if [[ "$(uname -s)" == "Darwin" ]] && [[ "$( grep -E 'nix-daemon.*fork()' /var/log/system.log | wc -l )" -gt 0 ]]; then
				>&2 echo ">>> I ran into a Mac-specific bug (https://github.com/NixOS/nix/issues/2523) and I'm applying the workaround automatically for it."
				sudo launchctl remove org.nixos.nix-daemon
				sed \
					-e "s:<key>Program</key>:<key>EnvironmentVariables</key><dict><key>OBJC_DISABLE_INITIALIZE_FORK_SAFETY</key><string>YES</string></dict>&:g" \
					/Library/LaunchDaemons/org.nixos.nix-daemon.plist > /tmp/org.nixos.nix-daemon.plist
				sudo mv /tmp/org.nixos.nix-daemon.plist /Library/LaunchDaemons/org.nixos.nix-daemon.plist
				sudo chown root: /Library/LaunchDaemons/org.nixos.nix-daemon.plist
				sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
				if ! nix-shell -p hello --run 'hello' &>/dev/null; then
					>&2 echo "ERR: The workaround was applied, but it's still not working!"
					exit 1
				fi
			else
				>&2 echo "ERR: Nix was installed but unable to use it. Running the command again for inspection."
				>&2 nix-shell -p hello --run 'hello'
				exit 1
			fi
		fi

		info "Installing nix-darwin"
		pushd "${workdir}"
			readonly nixpkgs_stable="$( nix-build --no-out-link "${shabka_path}/external/nixpkgs-stable.nix" )"
			readonly nixpkgs_unstable="$( nix-build --no-out-link -E "with import (import ${shabka_path}/external/nixpkgs-stable.nix) {}; import ${shabka_path}/external/nixpkgs-unstable.nix { inherit runCommand fetchpatch; }" )"
			readonly nixpkgs="${nixpkgs_unstable}"
			readonly nix_darwin="$( nix-build --no-out-link -E "with import ${nixpkgs} {}; import ${shabka_path}/external/nix-darwin.nix { inherit runCommand fetchpatch; }" )"
			set +e
				(yes | nix run -I darwin-config="${hostcf}/configuration.nix" -f "${nix_darwin}" --arg nixpkgs "${nixpkgs}" installer -c darwin-installer)
				RETVAL=$?
			set -e
			if [[ "${RETVAL}" -ne 0 ]] && [[ "${RETVAL}" -ne 141 ]]; then
				error "nix-darwin installer exited with status ${RETVAL}"
				exit "${RETVAL}"
			else
				if [[ -r /etc/static/bashrc ]]; then
					set +u
						source /etc/static/bashrc
					set -u
				else
					>&2 echo "ERR: Nix-Darwin was installed successfully, but was not able find /etc/static/bashrc"
				fi
			fi
		popd
	}

	# Wipe all (default) app icons from the Dock
	# This is only really useful when setting up a new Mac, or if you don’t use
	# the Dock to launch apps.
	defaults write com.apple.dock persistent-apps -array

	# record that we have bootstrapped so we do not try to bootstrap again
	defaults write com.github.kalbasit.shabka bootstrap -bool true
fi

# Brew the Brewfile
info "Brewing the Brew file"
while ! brew bundle --file="${here}/Brewfile" --verbose; do
	error "It looks like HomeBrew has failed, retry [y/n]"

	read -r answer
	while [[ -z "${answer}" ]] || ( [[ "${answer}" != "y" ]] && [[ "${answer}" != "n" ]] ); do
		error "I only understand y or n. Please respond with either y or n"
		read -r answer
	done

	if [[ "${answer}" == "n" ]]; then
		break
	fi
done

# Move zshrc and zprofile out of the way
if [[ -f /etc/zshrc ]] && [[ ! -f /etc/zshrc.orig ]]; then
	sudo mv /etc/zshrc /etc/zshrc.orig
fi
if [[ -f /etc/zprofile ]] && [[ ! -f /etc/zprofile.orig ]]; then
	sudo mv /etc/zprofile /etc/zprofile.orig
fi

# Finally, switch the generation
while ! "${shabka_path}/scripts/darwin-rebuild.sh" -h "${hostname}" switch; do
	error "It looks like Darwin Rebuild has failed, retry [y/n]"

	read -r answer
	while [[ -z "${answer}" ]] || ( [[ "${answer}" != "y" ]] && [[ "${answer}" != "n" ]] ); do
		error "I only understand y or n. Please respond with either y or n"
		read -r answer
	done

	if [[ "${answer}" == "n" ]]; then
		break
	fi
done

} # prevent the script from executing partially downloaded
