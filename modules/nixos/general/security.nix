{ pkgs, ... }:

let

  nasreddineCA = builtins.readFile (pkgs.fetchurl {
    url = "https://kalbas.it/ca.crt";
    sha256 = "17x45njva3a535czgdp5z43gmgwl0lk68p4mgip8jclpiycb6qbl";
  });

in {
  # hide process information of other users when running non-root
  security.hideProcessInformation = true;

  security.pki.certificates = [
    nasreddineCA
  ];
}
