{ vimUtils, fetchFromGitHub, stdenv }:

with vimUtils;

{
  vim-colemak = buildVimPluginFrom2Nix {
    name = "vim-colemak-2016-10-16";
    src = fetchFromGitHub {
      owner = "kalbasit";
      repo = "vim-colemak";
      rev = "6ac1c0bf362845355c65dfeab9a9987c1b4dc7ec";
      sha256 = "1li7yc5vglrhf7w7i7gs2i7ihdb1bhx85basmpgqlf7790lv1599";
    };
    dependencies = [];
  };

  vim-terraform = buildVimPluginFrom2Nix {
    name = "vim-terraform-2018-08-02";
    src = fetchFromGitHub {
      owner = "hashivim";
      repo = "vim-terraform";
      rev = "7c11252da45c6508524e022d1f2588134902d8d1";
      sha256 = "1qnjjcin934i7yd2fd0xapraindrpavnik1fasv10x5dw8yzxyrs";
    };
    dependencies = [];
  };

  vim-pig = buildVimPluginFrom2Nix {
    name = "vim-pig-2017-06-08";
    src = fetchFromGitHub {
      owner = "motus";
      repo = "pig.vim";
      rev = "60d8a0883d3e474e61af46b581a5ce3af65e9bb5";
      sha256  = "0az48a3slpzljb69d60cpahkshmdbss0snc8lmvf4yrc1gx8yncv";
    };
    dependencies = [];
  };

  vim-emmet = buildVimPluginFrom2Nix {
    name = "vim-emmet-2018-10-05";
    src = fetchFromGitHub {
      owner = "mattn";
      repo = "emmet-vim";
      rev = "7a4bf3463ef1e2c08393218fc67a8729c00948a5";
      sha256 = "15y5h7b6ll7nngaq9i44xb88rw2jg5ahbvybdn7kdf0nq1m3z409";
    };
    dependencies = [];
  };

  vim-yats = buildVimPluginFrom2Nix {
    name = "vim-yats-2018-10-16";
    src = fetchFromGitHub {
      owner = "HerringtonDarkholme";
      repo = "yats.vim";
      rev = "4675d7ff4b04aa5c5eabd5a1d862fcf78a7cd759";
      sha256 = "0k6q1x25shzkacmbk2wqgvvq74nx1r7rmhixhsm4vzz9917g22wv";
    };
    dependencies = [];
  };

  vim-typescript = buildVimPluginFrom2Nix {
    name = "vim-typescript-2018-11-05";
    src = fetchFromGitHub {
      owner = "mhartington";
      repo = "nvim-typescript";
      rev = "45bf956fb47c02eb2919e995191e115e1a7ffd76";
      sha256 = "0k6q1x25shzkacmbk2wqgvvq74nx1r7rmhixhsm4vzz9917g22wv";
    };
    dependencies = [];
  };

  # show trailing whitespace in red. It also strips whitespace on save (See
  # settings for it below). To disable it, use :ToggleStripWhitespaceOnSave and
  # to strip manually do :StripWhiteSpace
  vim-better-whitespace = buildVimPluginFrom2Nix {
    name = "vim-better-whitespace-2018-06-11";
    src = fetchFromGitHub {
      owner = "ntpeters";
      repo = "vim-better-whitespace";
      rev = "70a38fa9683e8cd0635264dd1b69c6ccbee4e3e7";
      sha256 = "1w16mrvydbvj9msi8p4ym1vasjx6kr4yd8jdhndz0pr3qasn2ix9";
    };
    dependencies = [];
  };

  vim-vissort = buildVimPluginFrom2Nix {
    name = "vim-vissort-2014-01-31";
    src = fetchFromGitHub {
      owner = "navicore";
      repo = "vissort.vim";
      rev = "75a5b08b64d2f762206bffd294066533891fa03c";
      sha256 = "0a71b22apkhicca9nkd06jlcnqkf583mlpfh2mvl4d474viavqfn";
    };
    dependencies = [];
  };

  vim-zoomwintab = buildVimPluginFrom2Nix {
    name = "vim-zoomwintab-2018-04-14";
    src = fetchFromGitHub {
      owner = "troydm";
      repo = "zoomwintab.vim";
      rev = "5bbbd1f79e40839a34803627e11f9e662f639fe0";
      sha256 = "04pv7mmlz9ccgzfg8sycqxplaxpbyh7pmhwcw47b2xwnazjz49d6";
    };
    dependencies = [];
  };

  vim-PreserveNoEOL = buildVimPluginFrom2Nix {
    name = "vim-PreserveNoEOL-2013-04-25";
    src = fetchFromGitHub {
      owner = "vim-scripts";
      repo = "PreserveNoEOL";
      rev = "940e3ce90e54d8680bec1135a21dcfbd6c9bfb62";
      sha256 = "1726jpr2zf6jrb00pp082ikbx4mll3a877pnzs6i18f9fgpaqqgd";
    };
    dependencies = [];
  };

  traces-vim = buildVimPluginFrom2Nix {
    name = "traces-vim-2018-10-14";
    src = fetchFromGitHub {
      owner = "markonm";
      repo = "traces.vim";
      rev = "9520ed3837340028b871a9e497dd0d0b07cb4953";
      sha256 = "0dfm04c4v0qk2f7fycpkwhbws0m5q383bizyaslflb1mmx3jnc48";
    };
    dependencies = [];
  };

  direnv-vim = buildVimPluginFrom2Nix {
    name = "direnv-vim-2017-12-29";
    src = fetchFromGitHub {
      owner = "direnv";
      repo = "direnv.vim";
      rev = "4d6271f0facd57a478c0d02895775dc01f577c5c";
      sha256 = "1vfg4hrxbqc96w694cn9gzjvwkscd111fp6dqlh7wpd2z3ciw07h";
    };
    dependencies = [];
  };
}
