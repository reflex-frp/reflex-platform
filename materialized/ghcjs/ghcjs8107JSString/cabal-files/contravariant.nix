{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { tagged = true; semigroups = true; statevar = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "contravariant"; version = "1.5.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2007-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/contravariant/";
      url = "";
      synopsis = "Contravariant functors";
      description = "Contravariant functors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).gt "7.10")) (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.9")) (hsPkgs."void" or (errorHandler.buildDepError "void"))) ++ (pkgs.lib).optional (flags.tagged && !(compiler.isGhc && (compiler.version).ge "7.7")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (flags.semigroups && !(compiler.isGhc && (compiler.version).ge "7.11")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.statevar) (hsPkgs."StateVar" or (errorHandler.buildDepError "StateVar"))) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/contravariant-1.5.3.tar.gz";
      sha256 = "44536f0e331fde471271937323dc90409e95d47f57e42657fdaf242a0fd65dc1";
      });
    }) // {
    package-description-override = "name:          contravariant\ncategory:      Control, Data\nversion:       1.5.3\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/contravariant/\nbug-reports:   http://github.com/ekmett/contravariant/issues\ncopyright:     Copyright (C) 2007-2015 Edward A. Kmett\nsynopsis:      Contravariant functors\ndescription:   Contravariant functors.\nbuild-type:    Simple\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\nextra-source-files:\n  .hlint.yaml\n  CHANGELOG.markdown\n  README.markdown\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/contravariant.git\n\nflag tagged\n  description:\n    You can disable the use of the `tagged` package using `-f-tagged`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag semigroups\n  description:\n    You can disable the use of the `semigroups` package using `-f-semigroups`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag StateVar\n  description:\n    You can disable the use of the `StateVar` package using `-f-StateVar`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nlibrary\n  hs-source-dirs: src\n  build-depends:\n    base                              < 5,\n    transformers        >= 0.3 &&     < 0.6\n\n  if !impl(ghc > 7.10)\n    build-depends: transformers-compat >= 0.5 && < 1\n\n  if !impl(ghc >= 7.9)\n    build-depends: void >= 0.6.1 && < 1\n\n  if flag(tagged) && !impl(ghc >= 7.7)\n    build-depends: tagged >= 0.8.6.1 && < 1\n\n  if flag(semigroups) && !impl(ghc >= 7.11)\n    build-depends: semigroups >= 0.18.5 && < 1\n\n  if flag(StateVar)\n    build-depends: StateVar >= 1.2.1 && < 1.3\n\n  if impl(ghc >= 7.2 && < 7.6)\n    build-depends: ghc-prim\n\n  exposed-modules:\n    Data.Functor.Contravariant.Compose\n    Data.Functor.Contravariant.Divisible\n\n  if impl(ghc < 8.5)\n    hs-source-dirs: old-src\n    exposed-modules: Data.Functor.Contravariant\n\n  if impl(ghc >= 7.4)\n    exposed-modules: Data.Functor.Contravariant.Generic\n\n  if impl(ghc >= 8.6)\n    ghc-options: -Wno-star-is-type\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\n  ghc-options: -Wall\n  default-language: Haskell2010\n";
    }