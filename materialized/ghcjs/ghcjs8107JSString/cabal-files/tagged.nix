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
    flags = { deepseq = true; transformers = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "tagged"; version = "0.8.6.1"; };
      license = "BSD-3-Clause";
      copyright = "2009-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/tagged";
      url = "";
      synopsis = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
      description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.5")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.6") (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))) ++ (pkgs.lib).optional (flags.deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))) ++ (pkgs.lib).optionals (flags.transformers) ([
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (if compiler.isGhc && (compiler.version).ge "7.10" || compiler.isGhcjs && true
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tagged-0.8.6.1.tar.gz";
      sha256 = "f5e0fcf95f0bb4aa63f428f2c01955a41ea1a42cfcf39145ed631f59a9616c02";
      });
    }) // {
    package-description-override = "name:           tagged\nversion:        0.8.6.1\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Edward A. Kmett\nmaintainer:     Edward A. Kmett <ekmett@gmail.com>\nstability:      experimental\ncategory:       Data, Phantom Types\nsynopsis:       Haskell 98 phantom types to avoid unsafely passing dummy arguments\nhomepage:       http://github.com/ekmett/tagged\nbug-reports:    http://github.com/ekmett/tagged/issues\ncopyright:      2009-2015 Edward A. Kmett\ndescription:    Haskell 98 phantom types to avoid unsafely passing dummy arguments.\nbuild-type:     Simple\ncabal-version:  >= 1.10\nextra-source-files: .hlint.yaml CHANGELOG.markdown README.markdown\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/tagged.git\n\nflag deepseq\n  description:\n    You can disable the use of the `deepseq` package using `-f-deepseq`.\n    .\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag transformers\n  description:\n    You can disable the use of the `transformers` and `transformers-compat` packages using `-f-transformers`.\n    .\n    Disable this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nlibrary\n  default-language: Haskell98\n  other-extensions: CPP\n  build-depends: base >= 2 && < 5\n  ghc-options: -Wall\n  hs-source-dirs: src\n  exposed-modules: Data.Tagged\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\n  if !impl(hugs)\n    cpp-options: -DLANGUAGE_DeriveDataTypeable\n    other-extensions: DeriveDataTypeable\n\n  if impl(ghc<7.7)\n    hs-source-dirs: old\n    exposed-modules: Data.Proxy\n    other-modules: Paths_tagged\n\n  if impl(ghc>=7.2 && <7.5)\n    build-depends: ghc-prim\n\n  if impl(ghc>=7.6)\n    exposed-modules: Data.Proxy.TH\n    build-depends: template-haskell >= 2.8 && < 2.18\n\n  if flag(deepseq)\n    build-depends: deepseq >= 1.1 && < 1.5\n\n  if flag(transformers)\n    build-depends: transformers        >= 0.2 && < 0.6\n\n    -- Ensure Data.Functor.Classes is always available\n    if impl(ghc >= 7.10) || impl(ghcjs)\n      build-depends: transformers >= 0.4.2.0\n    else\n      build-depends: transformers-compat >= 0.5 && < 1\n";
    }