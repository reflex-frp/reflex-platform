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
    flags = { integer-gmp = true; };
    package = {
      specVersion = "1.12";
      identifier = { name = "hashable"; version = "1.3.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Milan Straka <fox@ucw.cz>\nJohan Tibell <johan.tibell@gmail.com>";
      homepage = "http://github.com/haskell-unordered-containers/hashable";
      url = "";
      synopsis = "A class for types that can be converted to a hash value";
      description = "This package defines a class, 'Hashable', for types that\ncan be converted to a hash value.  This class\nexists for the benefit of hashing-based data\nstructures.  The package provides instances for\nbasic types and a way to combine hash values.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (if compiler.isGhc && (compiler.version).ge "9"
          then [
            (hsPkgs."ghc-bignum" or (errorHandler.buildDepError "ghc-bignum"))
            ]
          else if flags.integer-gmp
            then [
              (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
              ]
            else [
              (hsPkgs."integer-simple" or (errorHandler.buildDepError "integer-simple"))
              ]);
        buildable = true;
        };
      tests = {
        "hashable-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          };
        "hashable-examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hashable-1.3.1.0.tar.gz";
      sha256 = "8061823a4ac521b53912edcba36b956f3159cb885b07ec119af295a6568ca7c4";
      });
    }) // {
    package-description-override = "Cabal-version:       1.12\nName:                hashable\nVersion:             1.3.1.0\nSynopsis:            A class for types that can be converted to a hash value\nDescription:         This package defines a class, 'Hashable', for types that\n                     can be converted to a hash value.  This class\n                     exists for the benefit of hashing-based data\n                     structures.  The package provides instances for\n                     basic types and a way to combine hash values.\nHomepage:            http://github.com/haskell-unordered-containers/hashable\n-- SPDX-License-Identifier : BSD-3-Clause\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Milan Straka <fox@ucw.cz>\n                     Johan Tibell <johan.tibell@gmail.com>\nMaintainer:          Oleg Grenrus <oleg.grenrus@iki.fi>\nbug-reports:         https://github.com/haskell-unordered-containers/hashable/issues\nStability:           Provisional\nCategory:            Data\nBuild-type:          Simple\ntested-with:         GHC==8.10.3, GHC==8.8.3, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2\n\nExtra-source-files:\n  CHANGES.md, README.md\n\nFlag integer-gmp\n  Description: Are we using @integer-gmp@ to provide fast Integer instances? No effect on GHC-9.0 or later.\n  Default: True\n\nLibrary\n  Exposed-modules:   Data.Hashable\n                     Data.Hashable.Lifted\n                     Data.Hashable.Generic\n  Other-modules:     Data.Hashable.Class\n                     Data.Hashable.Generic.Instances\n\n  C-sources:         cbits/fnv.c\n  hs-source-dirs:    src\n\n  Build-depends:     base       >= 4.5      && < 4.16\n                   , bytestring >= 0.9      && < 0.12\n                   , deepseq    >= 1.3      && < 1.5\n                   , text       >= 0.12     && < 1.3\n                   , ghc-prim\n\n  if impl(ghc >= 9)\n    Build-depends:    ghc-bignum >= 1.0 && <1.1\n  else\n    if flag(integer-gmp)\n      Build-depends:   integer-gmp >= 0.4 && < 1.1\n    else\n      -- this is needed for the automatic flag to be well-balanced\n      Build-depends:   integer-simple\n\n  Default-Language:  Haskell2010\n  Other-Extensions:  BangPatterns\n                     CPP\n                     DeriveDataTypeable\n                     FlexibleContexts\n                     FlexibleInstances\n                     GADTs\n                     KindSignatures\n                     MagicHash\n                     MultiParamTypeClasses\n                     ScopedTypeVariables\n                     Trustworthy\n                     TypeOperators\n                     UnliftedFFITypes\n\n  Ghc-options:       -Wall -fwarn-tabs\n\nTest-suite hashable-tests\n  Type:              exitcode-stdio-1.0\n  Hs-source-dirs:    tests\n  Main-is:           Main.hs\n  Other-modules:     Properties Regress\n  Build-depends:     base,\n                     bytestring,\n                     ghc-prim,\n                     hashable,\n                     test-framework >= 0.3.3,\n                     test-framework-hunit,\n                     test-framework-quickcheck2 >= 0.2.9,\n                     HUnit,\n                     QuickCheck >= 2.4.0.1,\n                     random >= 1.0 && < 1.2,\n                     text >= 0.11.0.5\n  if !os(windows)\n    Build-depends:   unix\n    CPP-options:     -DHAVE_MMAP\n    Other-modules:   Regress.Mmap\n    Other-Extensions: CApiFFI\n\n  Ghc-options:       -Wall -fno-warn-orphans\n  Default-Language:  Haskell2010\n\ntest-suite hashable-examples\n  type:              exitcode-stdio-1.0\n  build-depends: base, hashable, ghc-prim\n  hs-source-dirs: examples\n  main-is: Main.hs\n  Default-Language:  Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-unordered-containers/hashable.git\n";
    }