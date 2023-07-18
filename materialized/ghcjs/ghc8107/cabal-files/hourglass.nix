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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "hourglass"; version = "0.2.12"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "vincent@snarc.org";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/vincenthz/hs-hourglass";
      url = "";
      synopsis = "simple performant time related library";
      description = "Simple time library focusing on simple but powerful and performant API\n\nThe backbone of the library are the Timeable and Time type classes.\n\nEach Timeable instances can be converted to type that has a Time instances,\nand thus are different representations of current time.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        };
      tests = {
        "test-hourglass" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-hourglass" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hourglass" or (errorHandler.buildDepError "hourglass"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hourglass-0.2.12.tar.gz";
      sha256 = "44335b5c402e80c60f1db6a74462be4ea29d1a9043aa994334ffee1164f1ca4a";
      });
    }) // {
    package-description-override = "Name:                hourglass\nVersion:             0.2.12\nSynopsis:            simple performant time related library\nDescription:\n    Simple time library focusing on simple but powerful and performant API\n    .\n    The backbone of the library are the Timeable and Time type classes.\n    .\n    Each Timeable instances can be converted to type that has a Time instances,\n    and thus are different representations of current time.\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          vincent@snarc.org\nCategory:            Time\nStability:           experimental\nBuild-Type:          Simple\nHomepage:            https://github.com/vincenthz/hs-hourglass\nCabal-Version:       >=1.10\nextra-source-files:  README.md\n                  ,  CHANGELOG.md\n                  ,  tests/TimeDB.hs\n\nLibrary\n  Exposed-modules:   Time.Types\n                   , Time.System\n                   , Time.Compat\n                   , Data.Hourglass\n                   , Data.Hourglass.Types\n                   , Data.Hourglass.Epoch\n                   , Data.Hourglass.Compat\n                   , System.Hourglass\n  Other-modules:     Data.Hourglass.Time\n                   , Data.Hourglass.Format\n                   , Data.Hourglass.Diff\n                   , Data.Hourglass.Local\n                   , Data.Hourglass.Calendar\n                   , Data.Hourglass.Zone\n                   , Data.Hourglass.Internal\n                   , Data.Hourglass.Utils\n  Build-depends:     base >= 4 && < 5\n                   , deepseq\n  ghc-options:       -Wall -fwarn-tabs\n  Default-Language:  Haskell2010\n  if os(windows)\n     cpp-options:    -DWINDOWS\n     Build-depends:  Win32\n     Other-modules:  Data.Hourglass.Internal.Win\n  else\n     Other-modules:  Data.Hourglass.Internal.Unix\n     c-sources:      cbits/unix.c\n\nTest-Suite test-hourglass\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    tests\n  Main-is:           Tests.hs\n  Build-Depends:     base >= 3 && < 5\n                   , mtl\n                   , tasty\n                   , tasty-quickcheck\n                   , tasty-hunit\n                   , hourglass\n                   , deepseq\n                   -- to test against some other reference\n                   , time\n                   , old-locale\n  ghc-options:       -Wall -fno-warn-orphans -fno-warn-missing-signatures\n  Default-Language:  Haskell2010\n  if os(windows)\n     cpp-options:    -DWINDOWS\n\nBenchmark bench-hourglass\n  hs-source-dirs:    tests\n  Main-Is:           Bench.hs\n  type:              exitcode-stdio-1.0\n  Default-Language:  Haskell2010\n  Build-depends:     base >= 4 && < 5\n                   , bytestring\n                   , gauge\n                   , mtl\n                   , deepseq\n                   , hourglass\n                   -- to benchmark against other reference\n                   , time\n                   , old-locale\n\nsource-repository head\n  type: git\n  location: https://github.com/vincenthz/hs-hourglass\n";
    }