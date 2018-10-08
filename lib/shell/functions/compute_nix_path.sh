function compute_nix_path() {
	local new_nix_path=""

	local system_path="$(cd $(dirname "${BASH_SOURCE[0]}")/../../.. && pwd)"

	# change the NIX_PATH to point to what we need in order to run nixos-rebuild
	new_nix_path="${HOME}/.nix-defexpr/channels"
	new_nix_path="${new_nix_path}:system-path=${system_path}"

	# add the nixos-config to the new_nix_path if /etc/NIXOS is found
	if [[ -f /etc/NIXOS ]]; then
		new_nix_path="${new_nix_path}:nixos-config=${system_path}/hosts/${machine}/configuration.nix"
	fi

	echo "${new_nix_path}"
}
