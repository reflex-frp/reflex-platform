let
  nixpkgs = import <nixpkgs> {};

  # The systems that we want to build for on the current system
  targets = if nixpkgs.stdenv.system == "x86_64-linux"
            # On linux, we want to build both 32-bit and 64-bit versions
            then [ "x86_64-linux" "i686-linux" ]
            else [ nixpkgs.stdenv.system ];

in nixpkgs.stdenv.lib.concatStringsSep " " targets
