let
  keys = builtins.fetchurl {
    url = "https://github.com/kalbasit.keys";
    sha256 = "1ka0wk5gb82hdfmz575c78j6pyqqshj4xqm37zizd1pxrl0fsifw";
  };
in
  keys
