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
      identifier = { name = "blaze-builder"; version = "0.4.2.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2010-2014 Simon Meier\n(c) 2010 Jasper Van der Jeugt\n(c) 2013-2015 Leon P Smith";
      maintainer = "Leon Smith <leon@melding-monads.com>";
      author = "Jasper Van der Jeugt, Simon Meier, Leon P Smith";
      homepage = "http://github.com/lpsmith/blaze-builder";
      url = "";
      synopsis = "Efficient buffered output.";
      description = "This library provides an implementation of the older\nblaze-builder interface in terms of the new builder that\nshipped with bytestring-0.10.4.0\n\nThis implementation is mostly intended as a bridge to the\nnew builder,  so that code that uses the old interface\ncan interoperate with code that uses the new\nimplementation.   Note that no attempt has been made\nto preserve the old internal modules,  so code that\nhas these dependencies cannot use this interface.\n\nNew code should,  for the most part,  use the new\ninterface.   However, this module does implement\na chunked HTTP encoding,  which is not otherwise\nimplemented (yet?) with the new builder.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (if compiler.isGhc && (compiler.version).lt "7.8"
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/blaze-builder-0.4.2.1.tar.gz";
      sha256 = "6e6889bc9c3ff92062a17f3825dcc1b28510d261334d4d4e177232d904ea0b06";
      });
    }) // {
    package-description-override = "Name:                blaze-builder\nVersion:             0.4.2.1\nSynopsis:            Efficient buffered output.\n\nDescription:\n                     This library provides an implementation of the older\n                     blaze-builder interface in terms of the new builder that\n                     shipped with bytestring-0.10.4.0\n                     .\n                     This implementation is mostly intended as a bridge to the\n                     new builder,  so that code that uses the old interface\n                     can interoperate with code that uses the new\n                     implementation.   Note that no attempt has been made\n                     to preserve the old internal modules,  so code that\n                     has these dependencies cannot use this interface.\n                     .\n                     New code should,  for the most part,  use the new\n                     interface.   However, this module does implement\n                     a chunked HTTP encoding,  which is not otherwise\n                     implemented (yet?) with the new builder.\n\nAuthor:              Jasper Van der Jeugt, Simon Meier, Leon P Smith\nCopyright:           (c) 2010-2014 Simon Meier\n                     (c) 2010 Jasper Van der Jeugt\n                     (c) 2013-2015 Leon P Smith\nMaintainer:          Leon Smith <leon@melding-monads.com>\n\nLicense:             BSD3\nLicense-file:        LICENSE\n\nHomepage:            http://github.com/lpsmith/blaze-builder\nBug-Reports:         http://github.com/lpsmith/blaze-builder/issues\nStability:           Experimental\n\nCategory:            Data\nBuild-type:          Simple\nCabal-version:       >= 1.10\n\nExtra-source-files:\n                     Makefile\n                     README.markdown\n                     TODO\n                     CHANGES\n\n                     benchmarks/*.hs\n                     benchmarks/Throughput/*.hs\n                     benchmarks/Throughput/*.h\n                     benchmarks/Throughput/*.c\n\n                     tests/*.hs\n\nSource-repository head\n  Type: git\n  Location: https://github.com/lpsmith/blaze-builder.git\n\nLibrary\n  ghc-options:       -Wall\n  default-language:  Haskell98\n\n  exposed-modules:   Blaze.ByteString.Builder\n                     Blaze.ByteString.Builder.Int\n                     Blaze.ByteString.Builder.Word\n                     Blaze.ByteString.Builder.ByteString\n                     Blaze.ByteString.Builder.Char.Utf8\n                     Blaze.ByteString.Builder.Char8\n                     Blaze.ByteString.Builder.Html.Utf8\n                     Blaze.ByteString.Builder.Html.Word\n                     Blaze.ByteString.Builder.HTTP\n                     Blaze.ByteString.Builder.Compat.Write\n\n                     Blaze.ByteString.Builder.Internal.Write\n\n  build-depends:     base == 4.* ,\n                     deepseq,\n                     text >= 0.10 && < 1.3\n\n  if impl(ghc < 7.8)\n     build-depends:  bytestring >= 0.9 && < 1.0,\n                     bytestring-builder\n  else\n     build-depends:  bytestring >= 0.10.4 && < 1.0\n\n  if impl(ghc < 8.0)\n     build-depends: semigroups >= 0.16 && < 0.20\n\ntest-suite test\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests\n  main-is:          Tests.hs\n  default-language: Haskell98\n  ghc-options:      -Wall -fno-warn-orphans\n\n  build-depends: base\n               , blaze-builder\n               , bytestring\n               , HUnit\n               , QuickCheck\n               , test-framework\n               , test-framework-hunit\n               , test-framework-quickcheck2\n               , text\n               , utf8-string\n";
    }