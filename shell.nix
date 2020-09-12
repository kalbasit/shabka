{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  name = "shabka";
  buildInputs = with pkgs; [
    git
    nixFlakes
  ];

  shellHook = ''
    PATH=${
      pkgs.writeShellScriptBin "nix" ''
        ${pkgs.nixFlakes}/bin/nix --option experimental-features "nix-command flakes ca-references" "$@"
      ''
    }/bin:$PATH
  '';

  SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
}
