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
      identifier = { name = "network-uri"; version = "2.6.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ezra@ezrakilty.net";
      author = "";
      homepage = "https://github.com/haskell/network-uri";
      url = "";
      synopsis = "URI manipulation";
      description = "This package provides facilities for parsing and unparsing URIs, and creating\nand resolving relative URI references, closely following the URI spec,\n<http://www.ietf.org/rfc/rfc3986.txt IETF RFC 3986>.\n\n== Backward-compatibility\n\nIn @network-2.6@ the \"Network.URI\" module was split off from the\n@network@ package into this package. If you're using the \"Network.URI\"\nmodule you can be backward compatible and automatically get it from\nthe right package by using the\n</package/network-uri-flag network-uri-flag pseudo-package>\nin your @.cabal@ file's build-depends (along with dependencies for\nboth @network-uri@ and @network@):\n\n>  build-depends:\n>     network-uri-flag == 0.1.*\n\nOr you can do the same manually by adding this boilerplate to your\n@.cabal@ file:\n\n> flag network-uri\n>   description: Get Network.URI from the network-uri package\n>   default: True\n>\n> library\n>   -- ...\n>   if flag(network-uri)\n>     build-depends: network-uri >= 2.6, network >= 2.6\n>   else\n>     build-depends: network-uri < 2.6, network < 2.6\n\nThat is, get the module from either @network < 2.6@ or from\n@network-uri >= 2.6@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "uri" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "uri-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-uri-2.6.4.1.tar.gz";
      sha256 = "57856db93608a4d419f681b881c9b8d4448800d5a687587dc37e8a9e0b223584";
      });
    }) // {
    package-description-override = "name:                network-uri\nversion:             2.6.4.1\nsynopsis:            URI manipulation\ndescription:\n  This package provides facilities for parsing and unparsing URIs, and creating\n  and resolving relative URI references, closely following the URI spec,\n  <http://www.ietf.org/rfc/rfc3986.txt IETF RFC 3986>.\n  .\n  == Backward-compatibility\n  .\n  In @network-2.6@ the \"Network.URI\" module was split off from the\n  @network@ package into this package. If you're using the \"Network.URI\"\n  module you can be backward compatible and automatically get it from\n  the right package by using the\n  </package/network-uri-flag network-uri-flag pseudo-package>\n  in your @.cabal@ file's build-depends (along with dependencies for\n  both @network-uri@ and @network@):\n  .\n  >  build-depends:\n  >     network-uri-flag == 0.1.*\n  .\n  Or you can do the same manually by adding this boilerplate to your\n  @.cabal@ file:\n  .\n  > flag network-uri\n  >   description: Get Network.URI from the network-uri package\n  >   default: True\n  >\n  > library\n  >   -- ...\n  >   if flag(network-uri)\n  >     build-depends: network-uri >= 2.6, network >= 2.6\n  >   else\n  >     build-depends: network-uri < 2.6, network < 2.6\n  .\n  That is, get the module from either @network < 2.6@ or from\n  @network-uri >= 2.6@.\n\nhomepage:            https://github.com/haskell/network-uri\nbug-reports:         https://github.com/haskell/network-uri/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nextra-source-files:  README.md, CHANGELOG.md\nmaintainer:          ezra@ezrakilty.net\ncategory:            Network\nbuild-type:          Simple\ncabal-version:       >=1.10\ntested-with:\n  GHC ==9.0.1\n   || ==8.10.1\n   || ==8.8.2\n   || ==8.6.5\n   || ==8.4.4\n   || ==8.2.2\n   || ==8.0.2\n   || ==7.10.3\n   || ==7.8.4\n   || ==7.6.3\n   || ==7.4.2\n   || ==7.2.2\n   || ==7.0.4\n\nlibrary\n  exposed-modules:\n    Network.URI\n    Network.URI.Lens\n    Network.URI.Static\n  build-depends:\n    base >= 3 && < 5,\n    deepseq >= 1.1 && < 1.5,\n    parsec >= 3.1.12.0 && < 3.2,\n    th-compat >= 0.1.1 && < 1.0\n  build-depends: template-haskell\n  default-extensions: CPP, DeriveDataTypeable\n  if impl(ghc < 7.6)\n    build-depends: ghc-prim\n  if impl(ghc >= 7.2)\n    default-extensions: DeriveGeneric\n  ghc-options: -Wall -fwarn-tabs\n  default-language: Haskell98\n\ntest-suite uri\n  hs-source-dirs: tests\n  main-is: uri001.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base < 5,\n    HUnit,\n    network-uri,\n    tasty,\n    tasty-hunit,\n    tasty-quickcheck,\n    QuickCheck\n\n  ghc-options: -Wall -fwarn-tabs\n  default-language: Haskell98\n\nbenchmark uri-bench\n  hs-source-dirs: tests\n  main-is: uri-bench.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base < 5,\n    HUnit,\n    network-uri,\n    criterion,\n    deepseq\n\n  ghc-options: -Wall -fwarn-tabs\n  default-language: Haskell98\n\nsource-repository head\n  type:     git\n  location: git://github.com/haskell/network-uri.git\n";
    }