#!/usr/bin/env bash

set -euo pipefail

readonly mthsbeVersion=e72d1060f3df8c157f93af52ea59508dae36ef50

if [[ "${#}" -ne 1 ]]; then
    echo "USAGE: $0 <hostname>"
    exit 1
fi

readonly hostname="${1}"
readonly here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly root="$( cd "${here}/../.." && pwd )"
readonly hostcf="${root}/hosts/${hostname}"

if ! defaults read com.github.kalbasit.shabka bootstrap >/dev/null 2>&1; then
    # Wipe all (default) app icons from the Dock
    defaults write com.apple.dock persistent-apps -array

    echo "[SHABKA] Installing Xcode command line tools"
    xcode-select --install || true

    # download and install Homebrew if it's not installed already
    command -v brew 2>/dev/null || {
        echo "[SHABKA] Installing HomeBrew"
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    }

    # download and install Nix
    command -v nix 2>/dev/null || {
        echo "[SHABKA] Installing Nix"
        curl https://nixos.org/nix/install | sh
        echo "source ~/.nix-profile/etc/profile.d/nix.sh" >> "${HOME}/.profile"
        source ~/.nix-profile/etc/profile.d/nix.sh
    }

    # Finally pull Nix if we already have a hostname
    if [[ -f "${hostcf}/home.nix" ]] && ! [[ -f "${HOME}/.config/home.nix" ]]; then
        echo "[SHABKA] Installing home-manager"
        mkdir -p "${HOME}/.config"
        ln -s "${hostcf}/home.nix" "${HOME}/.config/home.nix"

        readonly home_manager_nix_store="$(nix-instantiate --eval --read-write-mode "${root}/external/home-manager.nix" | cut -d\" -f2 | cut -d\" -f1)"
        HM_PATH="${home_manager_nix_store}" nix-shell "${HM_PATH}" -A install
    fi

    echo "[SHABKA] Be ready to type your sudo password"

    # Set computer name (as done via System Preferences → Sharing)
    echo "[SHABKA] Setting up the hostname"
    readonly hostnameInHex="$(echo -n "${hostname}" | od -A n -t x1 | tr -d ' ' | tr -d '\n' | sed 's/^/0x/')"
    sudo scutil --set ComputerName "${hostnameInHex}"
    sudo scutil --set HostName "${hostnameInHex}"
    sudo scutil --set LocalHostName "${hostnameInHex}"
    sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "${hostnameInHex}"

    # record that we have bootstrapped so we do not try to bootstrap again
    defaults write com.github.kalbasit.shabka bootstrap -bool true
fi

# Brew the Brewfile
echo "[SHABKA] Brewing the Brew file"
brew bundle --file="${here}/Brewfile" --verbose

# download the osx setup file from https://mths.be/macos
echo "[SHABKA] Downloading and running https://mths.be/macos"
readonly mthsbe="$(mktemp -d)"
rm -rf "${mthsbe}"
git clone https://github.com/mathiasbynens/dotfiles.git "${mthsbe}"
trap "rm -rf ${mthsbe}" EXIT
pushd "${mthsbe}"
    git reset --h "${mthsbeVersion}"
    sed -e "s@open '\$HOME/init/@open '${mthsbe}/init/@g" .macos > my-macos
    chmod +x my-macos && ./my-macos
popd

# run our own macos
"${here}/macos.sh"
if [[ -x "${hostcf}/macos.sh" ]]; then
    echo "[SHABKA] Running our own host-specific Darwin macos"
    "${hostcf}/macos.sh"
fi
