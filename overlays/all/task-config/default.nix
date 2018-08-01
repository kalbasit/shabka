{ stdenv, pkgs, ... }:

stdenv.mkDerivation rec {
  name = "task-config";

  phases = [ "installPhase" ];

  src = ./.;

  installPhase = ''
    install -dm755 $out/userHome

    substitute $src/taskrc $out/userHome/.taskrc \
      --subst-var-by taskwarrior_out ${pkgs.taskwarrior}
  '';
}
