#!/usr/bin/env bash
# vim: sw=2 ts=2 sts=0 noet

{ # prevent the script from executing partially downloaded

set -euo pipefail

readonly mthsbeVersion=e72d1060f3df8c157f93af52ea59508dae36ef50

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
readonly root="$( cd "${here}/../.." && pwd )"
readonly hostcf="${root}/hosts/${hostname}"
readonly workdir="$(mktemp -d)"
readonly deprecated_nixpkgs="${HOME}/.nixpkgs"
readonly xdg_config_nixpkgs="${HOME}/.config/nixpkgs"
trap "rm -rf ${workdir}" EXIT

sudo_prompt

if ! defaults read com.github.kalbasit.shabka bootstrap >/dev/null 2>&1; then
	# Wipe all (default) app icons from the Dock
	defaults write com.apple.dock persistent-apps -array

	info "Installing Xcode command line tools"
	xcode-select --install || true

	# download and install Homebrew if it's not installed already
	command -v brew 2>/dev/null || {
		info "Installing HomeBrew"
		/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	}

	# download and install Nix
	command -v nix 2>/dev/null || {
		mkdir -p "${xdg_config_nixpkgs}"
		mkdir -p "${deprecated_nixpkgs}"

		info "Installing Nix"
		curl https://nixos.org/nix/install | sh
		if ! grep -q nix.sh ~/.profile; then
			echo '. ~/.nix-profile/etc/profile.d/nix.sh' >> ~/.profile
		fi
		set +u
			source ~/.nix-profile/etc/profile.d/nix.sh
		set -u

		info "Installing nix-darwin"
		pushd "${deprecated_nixpkgs}"
			ln -sf "${hostcf}/darwin-configuration.nix" darwin-configuration.nix
		popd
		pushd "${workdir}"
			nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
			set +e
				(yes | ./result/bin/darwin-installer)
				RETVAL=$?
			set -e
			if [[ "${RETVAL}" -ne 0 ]] && [[ "${RETVAL}" -ne 141 ]]; then
				error "nix-darwin installer exited with status ${RETVAL}"
				exit "${RETVAL}"
			fi
			nix-channel --update darwin
		popd
	}

	# record that we have bootstrapped so we do not try to bootstrap again
	defaults write com.github.kalbasit.shabka bootstrap -bool true
fi

# Brew the Brewfile
info "Brewing the Brew file"
if ! brew bundle --file="${here}/Brewfile" --verbose; then
	info "It looks like HomeBrew has failed, please fix the issues (if any) and hit Enter to retry"
	info "NOTE: VirtualBox usually fails because of security issue, replaying error here"
	cat <<-EOF
	To install and/or use VirtualBox you may need to enable their kernel extension in

			System Preferences → Security & Privacy → General

	For more information refer to vendor documentation or the Apple Technical Note:

	https://developer.apple.com/library/content/technotes/tn2459/_index.html
	EOF

	read -r q
	brew bundle --file="${here}/Brewfile" --verbose
fi

# download the osx setup file from https://mths.be/macos
info "Downloading and running https://mths.be/macos"
readonly mthsbe="$(mktemp -d)"
rm -rf "${mthsbe}"
git clone https://github.com/mathiasbynens/dotfiles.git "${mthsbe}"
trap "rm -rf ${mthsbe}" EXIT
pushd "${mthsbe}"
git reset --h "${mthsbeVersion}"
sed -e "s@open '\$HOME/init/@open '${mthsbe}/init/@g" .macos > my-macos
chmod +x my-macos && ./my-macos
popd

} # prevent the script from executing partially downloaded
