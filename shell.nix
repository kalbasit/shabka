{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    gnumake
    nixops
    awscli
    (terraform.withPlugins (ps: [
      ps.aws
    ]))
  ];

  # Export the location of the SSL CA bundle
  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  NIX_SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
}
