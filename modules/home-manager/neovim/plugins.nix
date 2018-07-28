{ vimUtils, fetchFromGitHub }:

{
  vim-colemak = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-colemak-2016-10-16";
    src = fetchFromGitHub {
      owner = "kalbasit";
      repo = "vim-colemak";
      rev = "6ac1c0bf362845355c65dfeab9a9987c1b4dc7ec";
      sha256 = "1li7yc5vglrhf7w7i7gs2i7ihdb1bhx85basmpgqlf7790lv1599";
    };
    dependencies = [];
  };

  vim-terraform = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-terraform-2018-04-16";
    src = fetchFromGitHub {
      owner = "hashivim";
      repo = "vim-terraform";
      rev = "4e91b8c3a73fb9ecbf159fb5ca24ed6f39fad4f9";
      sha256 = "08qs61lll525ahwi1n4ksain04y830qd62sdxy3hjf6xjlppanzj";
    };
    dependencies = [];
  };

  vim-csv = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-csv-2018-06-24";
    src = fetchFromGitHub {
      owner = "chrisbra";
      repo = "csv.vim";
      rev = "918be3bd15920fd9bc79fca5e6870b8055742a1a";
      sha256 = "01fhw55s5q23ny3n7ldg53n3raysr2wnnkpfybbba2wv55w5vpdy";
    };
    dependencies = [];
  };

  vim-pig = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-pig-2017-06-08";
    src = fetchFromGitHub {
      owner = "motus";
      repo = "pig.vim";
      rev = "60d8a0883d3e474e61af46b581a5ce3af65e9bb5";
      sha256  = "0az48a3slpzljb69d60cpahkshmdbss0snc8lmvf4yrc1gx8yncv";
    };
    dependencies = [];
  };

  vim-emmet = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-emmet-2018-05-20";
    src = fetchFromGitHub {
      owner = "mattn";
      repo = "emmet-vim";
      rev = "f5b185e3a27d8db82b93bf7070387a332395abe1";
      sha256 = "1sl4qjy2afn8vspbf0qgfwffcxk4c6y6ngi2w3yda3bz3ssfrm8l";
    };
    dependencies = [];
  };

  vim-yats = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-yats-2018-06-20";
    src = fetchFromGitHub {
      owner = "HerringtonDarkholme";
      repo = "yats.vim";
      rev = "957a351b6ec33b06307c14a25195c73f28770959";
      sha256 = "1v0ckva8v3157087l9z3i07bq7h2n0b11fb8dn2xl1a6bgvxpy8n";
    };
    dependencies = [];
  };

  vim-color-seoul256 = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-color-seoul256-2017-09-04";
    src = fetchFromGitHub {
      owner = "junegunn";
      repo = "seoul256.vim";
      rev = "1475b7610663c68aa90b6e565997c8792ce0d222";
      sha256 = "03gqw14f5cirivcg1p06g500ns066yv5rd0z3zikvn4ql7n278dk";
    };
    dependencies = [];
  };

  vim-typescript = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-typescript-2018-06-05";
    src = fetchFromGitHub {
      owner = "mhartington";
      repo = "nvim-typescript";
      rev = "11074c8f6cf93c4714db2115a2c81cc9b6862acf";
      sha256 = "0yr9zmky2si663wpzvw26q6hz5kmzz016rnvz5p4lx7y56jwabad";
    };
    dependencies = [];
  };

   vim-go = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-go-2018-03-27";
    src = fetchFromGitHub {
      owner = "fatih";
      repo = "vim-go";
      rev = "v1.17";
      sha256 = "0syawx11mf66clsa049f4x3ajrsqmx4s4iy2fs184xp1d4n4qq8r";
    };
    dependencies = [];
  };

  # show trailing whitespace in red. It also strips whitespace on save (See
  # settings for it below). To disable it, use :ToggleStripWhitespaceOnSave and
  # to strip manually do :StripWhiteSpace
  vim-better-whitespace = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-better-whitespace-2018-06-11";
    src = fetchFromGitHub {
      owner = "ntpeters";
      repo = "vim-better-whitespace";
      rev = "70a38fa9683e8cd0635264dd1b69c6ccbee4e3e7";
      sha256 = "1w16mrvydbvj9msi8p4ym1vasjx6kr4yd8jdhndz0pr3qasn2ix9";
    };
    dependencies = [];
  };

  vim-vissort = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-vissort-2014-01-31";
    src = fetchFromGitHub {
      owner = "navicore";
      repo = "vissort.vim";
      rev = "75a5b08b64d2f762206bffd294066533891fa03c";
      sha256 = "0a71b22apkhicca9nkd06jlcnqkf583mlpfh2mvl4d474viavqfn";
    };
    dependencies = [];
  };

  vim-zoomwintab = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-zoomwintab-2018-04-14";
    src = fetchFromGitHub {
      owner = "troydm";
      repo = "zoomwintab.vim";
      rev = "5bbbd1f79e40839a34803627e11f9e662f639fe0";
      sha256 = "04pv7mmlz9ccgzfg8sycqxplaxpbyh7pmhwcw47b2xwnazjz49d6";
    };
    dependencies = [];
  };

  vim-PreserveNoEOL = vimUtils.buildVimPluginFrom2Nix {
    name = "vim-PreserveNoEOL-2013-04-25";
    src = fetchFromGitHub {
      owner = "vim-scripts";
      repo = "PreserveNoEOL";
      rev = "940e3ce90e54d8680bec1135a21dcfbd6c9bfb62";
      sha256 = "1726jpr2zf6jrb00pp082ikbx4mll3a877pnzs6i18f9fgpaqqgd";
    };
    dependencies = [];
  };
}
