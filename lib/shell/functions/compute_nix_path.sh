function compute_nix_path() {
	local new_nix_path=""

	local system_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../../.. && pwd)"

	# compute the store paths
	local nixpkgs_store_path="$(nix-store -r $( nix-instantiate "${system_path}/external/nixpkgs.nix" 2>/dev/null ) 2>/dev/null)"
	local nixos_hardware_store_path="$(nix-store -r $( nix-instantiate "${system_path}/external/nixos-hardware.nix" 2>/dev/null ) 2>/dev/null)"
	local home_manager_store_path="$(nix-store -r $( nix-instantiate "${system_path}/external/home-manager.nix" 2>/dev/null ) 2>/dev/null)"

	# change the NIX_PATH to point to what we need in order to run nixos-rebuild
	new_nix_path="home-manager=${home_manager_store_path}"
	new_nix_path="${new_nix_path}:nixpkgs-overlays=${system_path}/overlays"
	new_nix_path="${new_nix_path}:nixpkgs=${nixpkgs_store_path}"
	new_nix_path="${new_nix_path}:system-path=${system_path}"

	# add the nixos-config to the new_nix_path if /etc/NIXOS is found
	if [[ -f /etc/NIXOS ]]; then
		new_nix_path="${new_nix_path}:nixos-config=${system_path}/nixos/machines/${machine}/configuration.nix"
		new_nix_path="${new_nix_path}:nixos-hardware=${nixos_hardware_store_path}"
	fi

	echo "${new_nix_path}"
}
