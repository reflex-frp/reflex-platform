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
    flags = { useghc = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "vault"; version = "0.3.1.5"; };
      license = "BSD-3-Clause";
      copyright = "(c) Heinrich Apfelmus 2011-2013";
      maintainer = "Heinrich Apfelmus <apfelmus at quantentunnel de>";
      author = "Heinrich Apfelmus, Elliott Hird";
      homepage = "https://github.com/HeinrichApfelmus/vault";
      url = "";
      synopsis = "a persistent store for values of arbitrary types";
      description = "A /vault/ is a persistent store for values of arbitrary types.\nIt's like having first-class access to the storage space behind IORefs.\n\nThe data structure is analogous to a bank vault,\nwhere you can access different bank boxes with different keys;\nhence the name.\n\nAlso provided is a /locker/ type, representing a store for a single element.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vault-0.3.1.5.tar.gz";
      sha256 = "ac2a6b6adf58598c5c8faa931ae961a8a2aa50ddb2f0f7a2044ff6e8c3d433a0";
      });
    }) // {
    package-description-override = "Name:               vault\nVersion:            0.3.1.5\nSynopsis:           a persistent store for values of arbitrary types\nDescription:\n  A /vault/ is a persistent store for values of arbitrary types.\n  It's like having first-class access to the storage space behind IORefs.\n  .\n  The data structure is analogous to a bank vault,\n  where you can access different bank boxes with different keys;\n  hence the name.\n  .\n  Also provided is a /locker/ type, representing a store for a single element.\n\nCategory:           Data\nLicense:            BSD3\nLicense-file:       LICENSE\nAuthor:             Heinrich Apfelmus, Elliott Hird\nMaintainer:         Heinrich Apfelmus <apfelmus at quantentunnel de>\nHomepage:           https://github.com/HeinrichApfelmus/vault\nCopyright:          (c) Heinrich Apfelmus 2011-2013\n\nbuild-type:         Simple\ncabal-version:      >= 1.10\nTested-With:         GHC == 7.6.3\n                    ,GHC == 7.8.4\n                    ,GHC == 7.10.3\n                    ,GHC == 8.0.2\n                    ,GHC == 8.2.2\n                    ,GHC == 8.4.4\n                    ,GHC == 8.6.5\n                    ,GHC == 8.8.3\n                    ,GHC == 8.10.1\n\nextra-source-files:\n    CHANGELOG.md\n    README.md\n    src/Data/Vault/IO.h\n    src/Data/Vault/ST/ST.h\n    src/Data/Vault/ST/backends/GHC.h\n\nsource-repository head\n    type:           git\n    location:       git://github.com/HeinrichApfelmus/vault.git\n\nflag UseGHC\n    description: Use GHC-specific packages and extensions.\n    default:     True\n\nLibrary\n    hs-source-dirs:     src\n    build-depends:      base >= 4.5 && < 4.16,\n                        containers >= 0.4 && < 0.7,\n                        unordered-containers >= 0.2.3.0 && < 0.3,\n                        hashable >= 1.1.2.5 && < 1.4\n\n    if impl(ghc < 8.0)\n        build-depends:  semigroups >= 0.1 && < 1.0\n\n    default-language:   Haskell2010\n    default-extensions: CPP\n    ghc-options:        -Wall -fno-warn-missing-signatures\n\n    exposed-modules:\n                        Data.Vault.Lazy,\n                        Data.Vault.Strict,\n                        Data.Vault.ST.Lazy,\n                        Data.Vault.ST.Strict,\n                        Data.Unique.Really\n\n    if impl(ghc) && flag(UseGHC)\n        CPP-options:    -DUseGHC\n";
    }