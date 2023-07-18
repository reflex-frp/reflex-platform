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
    flags = { safe = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "void"; version = "0.7.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/void";
      url = "";
      synopsis = "A Haskell 98 logically uninhabited data type";
      description = "A Haskell 98 logically uninhabited data type, used to indicate that a given term should not exist.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.9")) ([
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/void-0.7.3.tar.gz";
      sha256 = "53af758ddc37dc63981671e503438d02c6f64a2d8744e9bec557a894431f7317";
      });
    }) // {
    package-description-override = "name:          void\ncategory:      Data Structures\nversion:       0.7.3\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     portable\nhomepage:      http://github.com/ekmett/void\nbug-reports:   http://github.com/ekmett/void/issues\ncopyright:     Copyright (C) 2008-2015 Edward A. Kmett\nsynopsis:      A Haskell 98 logically uninhabited data type\ndescription:   A Haskell 98 logically uninhabited data type, used to indicate that a given term should not exist.\nbuild-type:    Simple\ntested-with:   GHC==8.8.1\n             , GHC==8.6.5\n             , GHC==8.4.4\n             , GHC==8.2.2\n             , GHC==8.0.2\n             , GHC==7.10.3\n             , GHC==7.8.4\n             , GHC==7.6.3\n             , GHC==7.4.2\n             , GHC==7.2.2\n             , GHC==7.0.4\n\nextra-source-files:\n  .ghci\n  .gitignore\n  .travis.yml\n  .vim.custom\n  CHANGELOG.markdown\n  README.markdown\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/void.git\n\nflag safe\n  manual: True\n  default: False\n\nlibrary\n  default-language: Haskell98\n  hs-source-dirs: src\n  exposed-modules:\n    Data.Void.Unsafe\n\n  build-depends: base >= 3 && < 10\n\n  ghc-options: -Wall\n\n  if flag(safe)\n    cpp-options: -DSAFE\n\n  if !impl(ghc>=7.9)\n    hs-source-dirs: src-old\n    exposed-modules: Data.Void\n    build-depends:\n      deepseq    >= 1.1 && < 1.5,\n      hashable   >= 1.1,\n      semigroups >= 0.8.2,\n      template-haskell >=2.5.0.0 && <2.11\n\n    if impl(ghc)\n      other-extensions: DeriveDataTypeable\n      cpp-options: -DLANGUAGE_DeriveDataTypeable\n\n    if impl(ghc >= 7.2)\n      other-extensions: StandaloneDeriving\n      -- other-extensions: DeriveGeneric isn't known to cabal yet\n      cpp-options: -DLANGUAGE_DeriveGeneric\n      build-depends: ghc-prim\n";
    }