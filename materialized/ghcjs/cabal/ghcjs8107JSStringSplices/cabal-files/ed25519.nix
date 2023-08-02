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
    flags = {
      test-properties = true;
      test-hlint = true;
      test-doctests = true;
      no-donna = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "ed25519"; version = "0.0.5.0"; };
      license = "MIT";
      copyright = "Copyright (c) Austin Seipp 2013-2015";
      maintainer = "Austin Seipp <aseipp@pobox.com>";
      author = "Austin Seipp";
      homepage = "http://thoughtpolice.github.com/hs-ed25519";
      url = "";
      synopsis = "Ed25519 cryptographic signatures";
      description = "This package provides a simple, fast, self-contained copy of the\nEd25519 public-key signature system with a clean interface. It also\nincludes support for detached signatures, and thorough documentation\non the design and implementation, including usage guidelines.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = if flags.no-donna then true else false;
        };
      tests = {
        "properties" = {
          depends = (pkgs.lib).optionals (!(!flags.test-properties)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."ed25519" or (errorHandler.buildDepError "ed25519"))
            ];
          buildable = if !flags.test-properties then false else true;
          };
        "hlint" = {
          depends = (pkgs.lib).optionals (!(!flags.test-hlint)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hlint" or (errorHandler.buildDepError "hlint"))
            ];
          buildable = if !flags.test-hlint then false else true;
          };
        "doctests" = {
          depends = (pkgs.lib).optionals (!(!flags.test-doctests)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = if !flags.test-doctests then false else true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ed25519" or (errorHandler.buildDepError "ed25519"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ed25519-0.0.5.0.tar.gz";
      sha256 = "d8a5958ebfa9309790efade64275dc5c441b568645c45ceed1b0c6ff36d6156d";
      });
    }) // {
    package-description-override = "name:                ed25519\r\nversion:             0.0.5.0\r\nx-revision:          6\r\ncategory:            Cryptography\r\nlicense:             MIT\r\nsynopsis:            Ed25519 cryptographic signatures\r\nhomepage:            http://thoughtpolice.github.com/hs-ed25519\r\nbug-reports:         http://github.com/thoughtpolice/hs-ed25519/issues\r\nlicense-file:        LICENSE.txt\r\ncopyright:           Copyright (c) Austin Seipp 2013-2015\r\nauthor:              Austin Seipp\r\nmaintainer:          Austin Seipp <aseipp@pobox.com>\r\nbuild-type:          Simple\r\ncabal-version:       >=1.10\r\ntested-with:         GHC == 7.0.1, GHC == 7.0.2, GHC == 7.0.3, GHC == 7.0.4,\r\n                     GHC == 7.2.1, GHC == 7.2.2, GHC == 7.4.1, GHC == 7.4.2,\r\n                     GHC == 7.6.1, GHC == 7.6.2, GHC == 7.6.3,\r\n                     GHC == 7.8.2, GHC == 7.8.3, GHC == 7.8.4,\r\n                     GHC == 7.10.1, GHC == 7.10.2\r\n\r\ndescription:\r\n  This package provides a simple, fast, self-contained copy of the\r\n  Ed25519 public-key signature system with a clean interface. It also\r\n  includes support for detached signatures, and thorough documentation\r\n  on the design and implementation, including usage guidelines.\r\n\r\nextra-source-files:\r\n  .travis.yml\r\n  AUTHORS.txt\r\n  README.md\r\n  CONTRIBUTING.md\r\n  CHANGELOG.md\r\n  src/cbits/ref10/*.c\r\n  src/cbits/ref10/include/*.h\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/thoughtpolice/hs-ed25519.git\r\n\r\n-------------------------------------------------------------------------------\r\n-- Flags\r\n\r\nflag test-properties\r\n  default: True\r\n  manual: True\r\n\r\nflag test-hlint\r\n  default: True\r\n  manual: True\r\n\r\nflag test-doctests\r\n  default: True\r\n  manual: True\r\n\r\nflag no-donna\r\n  default: True\r\n  manual: True\r\n\r\n-------------------------------------------------------------------------------\r\n-- Build pt 1: main project\r\n\r\nlibrary\r\n  build-depends:\r\n    ghc-prim    >= 0.1 && < 0.10,\r\n    base        >= 4   && < 5,\r\n    bytestring  >= 0.9 && < 0.12\r\n\r\n  exposed-modules:\r\n    Crypto.Sign.Ed25519\r\n\r\n  ghc-options:        -Wall -fwarn-tabs\r\n  default-language:   Haskell2010\r\n  hs-source-dirs:     src\r\n\r\n  -- Choose the underlying C implementation\r\n  if flag(no-donna)\r\n    -- ref10 implementation from SUPERCOP, about 2x slower than the AMD64\r\n    -- SUPERCOP implementations, 15x faster than ronald3072 for signing.\r\n    c-sources:          src/cbits/ref10/ed25519.c\r\n    include-dirs:       src/cbits/ref10 src/cbits/ref10/include\r\n  else\r\n    -- TODO(aseipp): ed25519-donna import\r\n    buildable: False\r\n\r\n-------------------------------------------------------------------------------\r\n-- Build pt 2: Tests\r\n\r\ntest-suite properties\r\n  type: exitcode-stdio-1.0\r\n  main-is: properties.hs\r\n  ghc-options: -w\r\n  hs-source-dirs: tests\r\n  default-language:   Haskell2010\r\n\r\n  if !flag(test-properties)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n      base        >= 4   && < 5,\r\n      bytestring  >= 0.9 && < 0.12,\r\n      QuickCheck  >= 2.4 && < 2.9,\r\n      ed25519\r\n\r\n--\r\n-- Style/doc tests below\r\n--\r\n\r\ntest-suite hlint\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          hlint.hs\r\n  hs-source-dirs:   tests\r\n  default-language: Haskell2010\r\n\r\n  if !flag(test-hlint)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n      base  >= 4   && < 5,\r\n      hlint >= 1.7 && < 1.10\r\n\r\ntest-suite doctests\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          doctests.hs\r\n  hs-source-dirs:   tests\r\n  default-language: Haskell2010\r\n\r\n  if !flag(test-doctests)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n      base      >= 4    && < 5,\r\n      filepath  >= 1.0  && < 1.5,\r\n      directory >= 1.0  && < 1.3,\r\n      doctest   >= 0.10 && < 0.12\r\n\r\n-------------------------------------------------------------------------------\r\n-- Build pt 3: benchmarks\r\n\r\nbenchmark bench\r\n  type:               exitcode-stdio-1.0\r\n  build-depends:\r\n      base        >= 4   && < 5,\r\n      bytestring  >= 0.9 && < 0.12,\r\n      criterion   >= 0.8 && < 1.2,\r\n      deepseq     >= 1.3 && < 1.5,\r\n      ed25519\r\n\r\n  default-language:   Haskell2010\r\n  hs-source-dirs:     benchmarks\r\n  main-is:            bench.hs\r\n";
    }