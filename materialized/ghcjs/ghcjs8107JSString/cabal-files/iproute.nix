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
      identifier = { name = "iproute"; version = "1.7.11"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "http://www.mew.org/~kazu/proj/iproute/";
      url = "";
      synopsis = "IP Routing Table";
      description = "IP Routing Table is a tree of IP ranges\nto search one of them on the longest\nmatch base. It is a kind of TRIE with one\nway branching removed. Both IPv4 and IPv6\nare supported.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."appar" or (errorHandler.buildDepError "appar"))
          (hsPkgs."byteorder" or (errorHandler.buildDepError "byteorder"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."appar" or (errorHandler.buildDepError "appar"))
            (hsPkgs."byteorder" or (errorHandler.buildDepError "byteorder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ];
          buildable = true;
          };
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."appar" or (errorHandler.buildDepError "appar"))
            (hsPkgs."byteorder" or (errorHandler.buildDepError "byteorder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/iproute-1.7.11.tar.gz";
      sha256 = "205dcd27cce76345e4fc60060b5d428b015a09e9023f5f1bba58be1f562a8a8b";
      });
    }) // {
    package-description-override = "Name:                   iproute\nVersion:                1.7.11\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nHomepage:               http://www.mew.org/~kazu/proj/iproute/\nSynopsis:               IP Routing Table\nDescription:            IP Routing Table is a tree of IP ranges\n                        to search one of them on the longest\n                        match base. It is a kind of TRIE with one\n                        way branching removed. Both IPv4 and IPv6\n                        are supported.\nCategory:               Algorithms, Network\nCabal-Version:          >= 1.10\nBuild-Type:             Simple\nTested-With:            GHC == 7.8.4\n                      , GHC == 7.10.3\n                      , GHC == 8.0.2\n                      , GHC == 8.2.2\n                      , GHC == 8.4.4\n                      , GHC == 8.6.5\n                      , GHC == 8.8.2\n\nLibrary\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  Exposed-Modules:      Data.IP\n                        Data.IP.Builder\n                        Data.IP.Internal\n                        Data.IP.RouteTable\n                        Data.IP.RouteTable.Internal\n  Other-Modules:        Data.IP.Addr\n                        Data.IP.Mask\n                        Data.IP.Op\n                        Data.IP.Range\n  Build-Depends:        base >= 4.9 && < 5\n                      , appar\n                      , byteorder\n                      , bytestring\n                      , containers\n                      , network\n  if impl(ghc < 8.0)\n     Build-Depends:     semigroups >= 0.17\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n\nTest-Suite doctest\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  HS-Source-Dirs:       test\n  Ghc-Options:          -threaded -Wall\n  Main-Is:              doctests.hs\n  Build-Depends:        base >= 4.6 && < 5\n                      , doctest >= 0.9.3\n                      , appar\n                      , byteorder\n                      , bytestring\n                      , network\n\nTest-Suite spec\n  Type:                 exitcode-stdio-1.0\n  Default-Language:     Haskell2010\n  Hs-Source-Dirs:       test\n  Ghc-Options:          -Wall\n  Main-Is:              Spec.hs\n  Other-Modules:        RouteTableSpec\n                      , BuilderSpec\n                      , IPSpec\n  Build-Depends:        base >= 4.6 && < 5\n                      , hspec\n                      , QuickCheck\n                      , appar\n                      , byteorder\n                      , bytestring\n                      , containers\n                      , network\n                      , safe\n                      , iproute\n  if impl(ghc < 8.0)\n     Build-Depends:     semigroups >= 0.17\n\nSource-Repository head\n  Type:                 git\n  Location:             git://github.com/kazu-yamamoto/iproute.git\n";
    }