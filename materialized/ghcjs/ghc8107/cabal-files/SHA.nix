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
    flags = { exe = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "SHA"; version = "1.6.4.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Adam Wick <awick@galois.com>,\nRaphael Javaux <raphaeljavaux@gmail.com>";
      author = "Adam Wick <awick@galois.com>, Brian Lewis <brian@lorf.org>";
      homepage = "";
      url = "";
      synopsis = "Implementations of the SHA suite of message digest functions";
      description = "This library implements the SHA suite of message digest functions,\naccording to NIST FIPS 180-2 (with the SHA-224 addendum), as well\nas the SHA-based HMAC routines. The functions have been tested\nagainst most of the NIST and RFC test vectors for the various\nfunctions. While some attention has been paid to performance,\nthese do not presently reach the speed of well-tuned libraries,\nlike OpenSSL.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      exes = {
        "sha1" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        "sha224" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        "sha256" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        "sha384" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        "sha512" = {
          depends = (pkgs.lib).optionals (flags.exe) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            ];
          buildable = if flags.exe then true else false;
          };
        };
      tests = {
        "test-sha" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/SHA-1.6.4.4.tar.gz";
      sha256 = "6bd950df6b11a3998bb1452d875d2da043ee43385459afc5f16d471d25178b44";
      });
    }) // {
    package-description-override = "name:       SHA\ncategory:   Cryptography, Codec\nversion:    1.6.4.4\nlicense:    BSD3\nlicense-file: LICENSE\nauthor:     Adam Wick <awick@galois.com>, Brian Lewis <brian@lorf.org>\nmaintainer: Adam Wick <awick@galois.com>,\n            Raphael Javaux <raphaeljavaux@gmail.com>\nstability:  stable\nbuild-type: Simple\ncabal-version: >= 1.8\ntested-with: GHC == 7.6.1\nsynopsis: Implementations of the SHA suite of message digest functions\ndescription: This library implements the SHA suite of message digest functions,\n             according to NIST FIPS 180-2 (with the SHA-224 addendum), as well\n             as the SHA-based HMAC routines. The functions have been tested \n             against most of the NIST and RFC test vectors for the various\n             functions. While some attention has been paid to performance, \n             these do not presently reach the speed of well-tuned libraries, \n             like OpenSSL.\n\nFlag exe\n  Description: Build a sha executables similar to 'md5sum'.\n  Default: False\n\nLibrary\n  hs-source-dirs: src\n  build-depends: array > 0 && < 10000,\n                 base >= 4 && < 6,\n                 binary >= 0.7 && < 10000,\n                 bytestring > 0.8 && < 10000\n  exposed-modules: Data.Digest.Pure.SHA\n  GHC-Options: -Wall -fno-ignore-asserts -fno-warn-orphans\n               -funbox-strict-fields -fwarn-tabs\n  extensions: BangPatterns\n  if impl(ghc >= 6.12) && impl(ghc < 7.7)\n    Ghc-Options: -fregs-graph\n\ntest-suite test-sha\n  type:            exitcode-stdio-1.0\n  hs-source-dirs:  src\n  main-is:         Test.hs\n  ghc-options:     -Wall\n  build-depends: array > 0 && < 10000,\n                 base > 4.3 && < 7,\n                 binary >= 0.7 && < 10000,\n                 bytestring > 0.8 && < 10000,\n                 QuickCheck >= 2.5 && < 3,\n                 test-framework >= 0.8.0.3 && < 10000,\n                 test-framework-quickcheck2 >= 0.3.0.2 && < 10000\n  extensions: BangPatterns, MultiParamTypeClasses, DeriveDataTypeable\n  GHC-Options: -O2 -Wall -fno-ignore-asserts -fno-warn-orphans\n               -funbox-strict-fields -fwarn-tabs\n  cpp-options: -DSHA_TEST\n  if impl(ghc >= 6.12) && impl(ghc < 7.7)\n    Ghc-Options: -fregs-graph\n\nExecutable sha1\n  Main-Is: Main.hs\n  if flag(exe)\n    hs-source-dirs: src-bin\n    build-depends: base >= 4 && < 6,\n                   bytestring > 0.8 && < 10000,\n                   directory > 0.0 && < 10000,\n                   SHA > 1.6 && < 10000\n    extensions: CPP\n    GHC-Options: -O2 -Wall -fno-ignore-asserts -fno-warn-orphans\n                 -funbox-strict-fields -fwarn-tabs\n    cpp-options: -DALGORITHM=sha1\n    if impl(ghc >= 6.12) && impl(ghc < 7.7)\n      Ghc-Options: -fregs-graph\n  else \n    buildable: False\n\nExecutable sha224\n  Main-Is: Main.hs\n  if flag(exe)\n    hs-source-dirs: src-bin\n    build-depends: base >= 4 && < 6,\n                   bytestring > 0.8 && < 10000,\n                   directory > 0.0 && < 10000,\n                   SHA > 1.6 && < 10000\n    extensions: CPP\n    GHC-Options: -O2 -Wall -fno-ignore-asserts -fno-warn-orphans\n                 -funbox-strict-fields -fwarn-tabs\n    cpp-options: -DALGORITHM=sha224\n    if impl(ghc >= 6.12) && impl(ghc < 7.7)\n      Ghc-Options: -fregs-graph\n  else\n    buildable: False\n\nExecutable sha256\n  Main-Is: Main.hs\n  if flag(exe)\n    hs-source-dirs: src-bin\n    build-depends: base >= 4 && < 6,\n                   bytestring > 0.8 && < 10000,\n                   directory > 0.0 && < 10000,\n                   SHA > 1.6 && < 10000\n    extensions: CPP\n    GHC-Options: -O2 -Wall -fno-ignore-asserts -fno-warn-orphans\n                 -funbox-strict-fields -fwarn-tabs\n    cpp-options: -DALGORITHM=sha256\n    if impl(ghc >= 6.12) && impl(ghc < 7.7)\n      Ghc-Options: -fregs-graph\n  else\n    buildable: False\n\nExecutable sha384\n  Main-Is: Main.hs\n  if flag(exe)\n    hs-source-dirs: src-bin\n    build-depends: base >= 4 && < 6,\n                   bytestring > 0.8 && < 10000,\n                   directory > 0.0 && < 10000,\n                   SHA > 1.6 && < 10000\n    extensions: CPP\n    GHC-Options: -O2 -Wall -fno-ignore-asserts -fno-warn-orphans\n                 -funbox-strict-fields -fwarn-tabs\n    cpp-options: -DALGORITHM=sha384\n    if impl(ghc >= 6.12) && impl(ghc < 7.7)\n      Ghc-Options: -fregs-graph\n  else\n    buildable: False\n\nExecutable sha512\n  Main-Is: Main.hs\n  if flag(exe)\n    hs-source-dirs: src-bin\n    build-depends: base >= 4 && < 6,\n                   bytestring > 0.8 && < 10000,\n                   directory > 0.0 && < 10000,\n                   SHA > 1.6 && < 10000\n    extensions: CPP\n    GHC-Options: -O2 -Wall -fno-ignore-asserts -fno-warn-orphans\n                 -funbox-strict-fields -fwarn-tabs\n    cpp-options: -DALGORITHM=sha512\n    if impl(ghc >= 6.12) && impl(ghc < 7.7)\n      Ghc-Options: -fregs-graph\n  else\n    buildable: False\n\nsource-repository head\n  type:     git\n  location: git://github.com/GaloisInc/SHA.git\n";
    }