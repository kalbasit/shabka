{ stdenv, fetchFromGitHub, ... }:

stdenv.mkDerivation rec {
  name = "kubetail-${version}";
  version = "1.6.1";

  src = fetchFromGitHub {
    owner = "johanhaleby";
    repo = "kubetail";
    rev = "${version}";
    sha256 = "10ql1kdsmyrk73jb6f5saf2q38znm0vdihscj3c9n0qhyhk9blpl";
  };

  installPhase = ''
    install -Dm755 kubetail $out/bin/kubetail
  '';
}
