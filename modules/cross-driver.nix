# NOTE: Cross Driver
# This sets up a <system> that has splice loading enabled
# We currently use a "splice-driver" to do all of the dirty work regarding setting-up "preBuild"
# to load splices

# We also generate a new package-set here with "project'" from haskell.nix since passing through the plan-pkgs doesn't always work 100% of the time
# when building for crossPlatforms
{ name,
  src,
  crossPkgs,
  splice-driver,
  hardening-driver,
  compiler-nix-name,
  overrides ? [ ],
  pkg-set,
  spliced-packages ? pkg-set,
  extra-hackage-tarballs ? { },
  extra-hackages ? [ ],
  inputMap,
  sha256map,
  ...
}@args: let
  # Should probably be an inherit list with a mapping from (isiOS -> isIos)
  filterStdenv = attrs: builtins.listToAttrs (builtins.concatMap (a: if crossPkgs.lib.hasPrefix "is" a then [{ name = if a == "isiOS" then "isIos" else a; value = attrs.${a}; }] else []) (builtins.attrNames attrs));
  inherit (crossPkgs) lib stdenv;

  # NOTE: We Double Eval here (base-pkg-set -> cross-pkg-set)
  # this is due to some weird interactions with the cabal solver depending on the
  # cross target that you're targeting
  # TODO: Take a close look into this and figure out what is getting jumbled between
  # the transition
in crossPkgs.haskell-nix.project' {
  inherit name compiler-nix-name;
  # NOTE: The way that haskell.nix resolves the ghcjs compiler is super weird, so default to ghc(js)8107 on any platform
  # that uses ghcjs
  src = crossPkgs.haskell-nix.haskellLib.cleanGit {
    inherit name src;
  };

  inherit inputMap sha256map;

  # Include our custom hackage that is generated by overrides
  inherit extra-hackages extra-hackage-tarballs;

  # If you want to change your global hackage index-state
  index-state = args.index-state or null;

  modules = [
    # NOTE: Set compiler properly
    ({ config, lib, ... }: {
      config.compiler.nix-name = lib.mkForce (compiler-nix-name);
    })
    # NOTE (@cidkidnix):
    # Properly set cabal system arguments for the generated plan-nix,
    # currently haskell.nix doesn't do this properly and it ends up in
    # weird cases where some things aren't properly included to be built
    { cabal.system = filterStdenv stdenv.hostPlatform; }
    { config.ghcOptions = [ "-fexpose-all-unfoldings" ]; }
    /*{
      config.reinstallableLibGhc = lib.mkForce false;
      config.nonReinstallablePkgs = lib.mkForce [
          "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base" "deepseq"
          "array" "ghc-boot-th" "pretty" "template-haskell" "ghcjs-prim" "ghcjs-th" "ghc-boot"
          "ghc" "Win32" "array" "binary" "bytestring" "containers" "directory" "filepath" "ghc-boot"
          "ghc-compact" "ghc-prim" "hpc" "mtl" "parsec" "process" "text" "time" "transformers"
          "unix" "xhtml" "terminfo"
      ];
      }
      */
  ] ++ overrides
  ++ lib.optionals (stdenv.hostPlatform.isiOS) ([
    {
      packages."${name}".ghcOptions = [
        "-fwhole-archive-hs-libs"
      ];
    }
  ])
    # Disable some stuff to make ghcjs properly function
    # since haskell.nix enables too strict of rules by default
    ++ lib.optionals (stdenv.targetPlatform.isGhcjs) ([
      {
        config.doExactConfig = lib.mkForce false;
        config.reinstallableLibGhc = lib.mkForce false;
        # We add modified packages to the compiler so we have to add them here
        config.nonReinstallablePkgs = lib.mkForce [
          "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base" "deepseq"
          "array" "ghc-boot-th" "pretty" "template-haskell" "ghcjs-prim" "ghcjs-th" "ghc-boot"
          "ghc" "Win32" "array" "binary" "bytestring" "containers" "directory" "filepath" "ghc-boot"
          "ghc-compact" "ghc-prim" "hpc" "mtl" "parsec" "process" "text" "time" "transformers"
          "unix" "xhtml" "terminfo"
          # Our stuff
          "ghcjs-base" "primitive" "dlist" "vector"
        ];
      }
    ] ++ lib.optionals (compiler-nix-name == "ghcjs8107JSString") crossPkgs.obsidianCompilers.jsstring-overrides)

    # Do this if were not on ghcjs
    ++ lib.optionals (!stdenv.targetPlatform.isGhcjs) ([
        ({ config, lib, ... }: { packages.Cabal.patches = lib.mkForce [ ]; })
      ]
    # NOTE: Use the splice driver to setup the loading side of splices
    # refer to ./splice-driver.nix
    ++ lib.optionals (crossPkgs.stdenv.hostPlatform != crossPkgs.stdenv.buildPlatform) (splice-driver {
    attrs = pkg-set.config.packages;
    string = (aname: cname: subname:
      if cname == "library" then ''
        echo "!!! Loading Splices ${spliced-packages.config.hsPkgs.${aname}.components.library}/lib/haskell.nix/$pname"
        export EXTERNAL_SPLICES_LOAD="${spliced-packages.config.hsPkgs.${aname}.components.library}/lib/haskell.nix/$pname"
      '' else ''
        echo "!!! Loading Splices ${spliced-packages.config.hsPkgs.${aname}.components.${cname}.${subname}}/lib/haskell.nix/$pname"
        export EXTERNAL_SPLICES_LOAD="${spliced-packages.config.hsPkgs.${aname}.components.${cname}.${subname}}/lib/haskell.nix/$pname"
      '');
    }) ++ (hardening-driver {
      attrs = pkg-set.config.packages;
    }));
}
