import ((import ./fetchNixpkgs.nix) { inherit (builtins.fromJSON (builtins.readFile ./github.json)) rev sha256 sha256unpacked; })
