source "$(dirname ${BASH_SOURCE[0]})/get_url.sh"
source "$(dirname ${BASH_SOURCE[0]})/get_rev.sh"

# TODO: this function should use nix to compute the NIX_PATH using local values
# from the store, similar to nixos/cfg/nix.nix
function compute_nix_path() {
	local nix_path=""

	local system_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../../.. && pwd)"

	# compute the urls
	local nixpkgs_archive="$( get_url ${system_path}/external/nixpkgs-version.json )/archive/$( get_rev ${system_path}/external/nixpkgs-version.json).tar.gz"
	local nixos_hardware_archive="$( get_url ${system_path}/external/nixos-hardware-version.json )/archive/$( get_rev ${system_path}/external/nixos-hardware-version.json).tar.gz"
	local home_manager_archive="$( get_url ${system_path}/external/home-manager-version.json )/archive/$( get_rev ${system_path}/external/home-manager-version.json).tar.gz"

	# change the NIX_PATH to point to what we need in order to run nixos-rebuild
	nix_path="home-manager=${home_manager_archive}"
	nix_path="${nix_path}:nixos-config=${system_path}/nixos/machines/${machine}/configuration.nix"
	nix_path="${nix_path}:nixos-hardware=${nixos_hardware_archive}"
	nix_path="${nix_path}:nixpkgs-overlays=${system_path}/overlays"
	nix_path="${nix_path}:nixpkgs=${nixpkgs_archive}"
	nix_path="${nix_path}:system-path=${system_path}"

	echo $nix_path
}

