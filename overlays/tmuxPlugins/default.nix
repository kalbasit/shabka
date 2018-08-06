self: super:

let
  rtpPath = "share/tmux-plugins";

  addRtp = path: rtpFileName: attrs: derivation:
    derivation // { rtp = "${derivation}/${path}/${rtpFileName}"; } // {
      overrideAttrs = f: buildTmuxPlugin (attrs // f attrs);
    };

  buildTmuxPlugin = a@{
    pluginName,
    rtpFileName ? (builtins.replaceStrings ["-"] ["_"] pluginName) + ".tmux",
    namePrefix ? "tmuxplugin-",
    src,
    unpackPhase ? "",
    configurePhase ? "",
    buildPhase ? "",
    addonInfo ? null,
    preInstall ? "",
    postInstall ? "",
    path ? (builtins.parseDrvName pluginName).name,
    dependencies ? [],
    ...
  }:
    addRtp "${rtpPath}/${path}" rtpFileName a (self.stdenv.mkDerivation (a // {
      name = namePrefix + pluginName;

      inherit pluginName unpackPhase configurePhase buildPhase addonInfo preInstall postInstall;

      installPhase = ''
        runHook preInstall
        target=$out/${rtpPath}/${path}
        mkdir -p $out/${rtpPath}
        cp -r . $target
        if [ -n "$addonInfo" ]; then
          echo "$addonInfo" > $target/addon-info.json
        fi
        runHook postInstall
      '';

      dependencies = [ self.bash ] ++ dependencies;
    }));

  buildTmuxPluginFrom2Nix = a: buildTmuxPlugin ({
    buildPhase = ":";
    configurePhase =":";
  } // a);
in {
  myTmuxPlugins.fzf-tmux-url = buildTmuxPluginFrom2Nix {
    pluginName = "fzf-tmux-url";
    rtpFileName = "fzf-url.tmux";
    src = self.fetchgit {
      url = "https://github.com/wfxr/tmux-fzf-url";
      rev = "ecd518eec1067234598c01e655b048ff9d06ef2f";
      sha256 = "0png8hdv91y2nivq5vdii2192mb2qcrkwwn69lzxrdnbfa27qrgv";
    };
  };
}
