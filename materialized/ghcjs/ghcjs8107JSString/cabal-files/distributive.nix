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
    flags = { semigroups = true; tagged = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "distributive"; version = "0.6.2.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/distributive/";
      url = "";
      synopsis = "Distributive functors -- Dual to Traversable";
      description = "Distributive functors -- Dual to @Traversable@";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8.0") ((pkgs.lib).optional (flags.semigroups) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups")));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/distributive-0.6.2.1.tar.gz";
      sha256 = "d7351392e078f58caa46630a4b9c643e1e2e9dddee45848c5c8358e7b1316b91";
      });
    }) // {
    package-description-override = "name:          distributive\ncategory:      Data Structures\nversion:       0.6.2.1\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/distributive/\nbug-reports:   http://github.com/ekmett/distributive/issues\ncopyright:     Copyright (C) 2011-2016 Edward A. Kmett\nsynopsis:      Distributive functors -- Dual to Traversable\ndescription:   Distributive functors -- Dual to @Traversable@\nbuild-type:    Simple\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.4\n             , GHC == 8.10.3\nextra-source-files:\n  .hlint.yaml\n  .vim.custom\n  config\n  CHANGELOG.markdown\n  README.markdown\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/distributive.git\n\nflag semigroups\n  manual: True\n  default: True\n  description:\n    You can disable the use of the `semigroups` package using `-f-semigroups`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n\nflag tagged\n  manual: True\n  default: True\n  description:\n    You can disable the use of the `tagged` package using `-f-tagged`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n\nlibrary\n  build-depends:\n    base                >= 4   && < 5,\n    base-orphans        >= 0.5.2 && < 1,\n    transformers        >= 0.3 && < 0.6\n\n  hs-source-dirs:  src\n  exposed-modules:\n    Data.Distributive\n\n  if impl(ghc>=7.2)\n    exposed-modules: Data.Distributive.Generic\n\n  if flag(tagged)\n    build-depends: tagged >= 0.7 && < 1\n\n  if impl(ghc>=7.2 && < 7.6)\n    build-depends: ghc-prim\n\n  if impl(ghc < 8.0)\n    if flag(semigroups)\n      build-depends: semigroups >= 0.13 && < 1\n\n  if impl(ghc < 7.8)\n    hs-source-dirs: src-compat\n    other-modules: Data.Coerce\n\n  ghc-options: -Wall\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\n  default-language: Haskell2010\n\ntest-suite spec\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: tests\n  build-tool-depends:\n    hspec-discover:hspec-discover\n\n  build-depends:\n    base             >= 4    && < 5,\n    distributive,\n    generic-deriving >= 1.11 && < 2,\n    hspec            >= 2    && < 3\n\n  main-is: Spec.hs\n  other-modules: GenericsSpec\n\n  ghc-options: -Wall -threaded -rtsopts\n  default-language: Haskell2010\n";
    }