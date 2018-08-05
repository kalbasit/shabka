{ pkgs, stdenv, ... }:

stdenv.mkDerivation rec {
  name = "zsh-functions-${version}";
  version = "0.0.1";
  src = ./.;
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir $out

    cp $src/* $out/

    rm -f $out/default.nix

    substituteInPlace $out/get_pr \
      --subst-var-by curl_bin ${pkgs.curl}/bin/curl \
      --subst-var-by git_bin ${pkgs.git}/bin/git \
      --subst-var-by jq_bin ${pkgs.jq}/bin/jq \
      --subst-var-by xsel_bin ${pkgs.xsel}/bin/xsel

    substituteInPlace $out/git_gopath_formatted_repo_path \
      --subst-var-by git_bin ${pkgs.git}/bin/git \
      --subst-var-by perl_bin ${pkgs.perl}/bin/perl

    substituteInPlace $out/jsonpp \
      --subst-var-by python_bin ${pkgs.python37Full}/bin/python \
      --subst-var-by pygmentize_bin ${pkgs.python36Packages.pygments}/bin/pygmentize

    substituteInPlace $out/jspp \
      --subst-var-by js-beautify_bin ${pkgs.python36Packages.jsbeautifier}/bin/js-beautify

    substituteInPlace $out/new_pr \
      --subst-var-by curl_bin ${pkgs.curl}/bin/curl \
      --subst-var-by git_bin ${pkgs.git}/bin/git \
      --subst-var-by jq_bin ${pkgs.jq}/bin/jq \
      --subst-var-by xsel_bin ${pkgs.xsel}/bin/xsel

    substituteInPlace $out/tmycli \
      --subst-var-by mycli_bin ${pkgs.mycli}/bin/mycli \
      --subst-var-by netstat_bin ${pkgs.nettools}/bin/netstat \
      --subst-var-by ssh_bin ${pkgs.openssh}/bin/ssh

    substituteInPlace $out/pet_select \
      --subst-var-by pet_bin ${pkgs.pet}/bin/pet

    substituteInPlace $out/pet_prev \
      --subst-var-by pet_bin ${pkgs.pet}/bin/pet

    substituteInPlace $out/pr \
      --subst-var-by git_bin ${pkgs.git}/bin/git

    substituteInPlace $out/xmlpp \
      --subst-var-by xmllint_bin ${pkgs.libxml2Python}/bin/xmllint
  '';
}
