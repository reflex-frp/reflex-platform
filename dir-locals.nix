let pkgs = import <nixpkgs> {};
    reflex-platform = import ./. {};
in pkgs.nixBufferBuilders.withPackages
     (builtins.filter (p: p != null) reflex-platform.tryReflexPackages)
