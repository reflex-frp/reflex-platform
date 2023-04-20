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
      network-bytestring = false;
      allow-sendfilefd = true;
      warp-debug = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "warp"; version = "3.3.14"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Kazu Yamamoto, Matt Brown";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "A fast, light-weight web server for WAI applications.";
      description = "HTTP\\/1.0, HTTP\\/1.1 and HTTP\\/2 are supported.\nFor HTTP\\/2,  Warp supports direct and ALPN (in TLS)\nbut not upgrade.\nAPI docs and the README are available at\n<http://www.stackage.org/package/warp>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."bsb-http-chunked" or (errorHandler.buildDepError "bsb-http-chunked"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."simple-sendfile" or (errorHandler.buildDepError "simple-sendfile"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if flags.network-bytestring
          then [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-bytestring" or (errorHandler.buildDepError "network-bytestring"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ])) ++ (if system.isWindows
          then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = false;
          };
        "spec" = {
          depends = (([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
            (hsPkgs."bsb-http-chunked" or (errorHandler.buildDepError "bsb-http-chunked"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."simple-sendfile" or (errorHandler.buildDepError "simple-sendfile"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8") [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]) ++ (pkgs.lib).optional ((system.isLinux || system.isFreebsd || system.isOsx) && flags.allow-sendfilefd) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."time" or (errorHandler.buildDepError "time"));
          buildable = true;
          };
        };
      benchmarks = {
        "parser" = {
          depends = (([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional ((system.isLinux || system.isFreebsd || system.isOsx) && flags.allow-sendfilefd) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."time" or (errorHandler.buildDepError "time"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/warp-3.3.14.tar.gz";
      sha256 = "2331da1ac67c644828883498301bee7bbf59f8b3d79b37850a621cba9a811572";
      });
    }) // {
    package-description-override = "Name:                warp\nVersion:             3.3.14\nSynopsis:            A fast, light-weight web server for WAI applications.\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman, Kazu Yamamoto, Matt Brown\nMaintainer:          michael@snoyman.com\nHomepage:            http://github.com/yesodweb/wai\nCategory:            Web, Yesod\nBuild-Type:          Simple\nCabal-Version:       >= 1.10\nStability:           Stable\ndescription:         HTTP\\/1.0, HTTP\\/1.1 and HTTP\\/2 are supported.\n                     For HTTP\\/2,  Warp supports direct and ALPN (in TLS)\n                     but not upgrade.\n                     API docs and the README are available at\n                     <http://www.stackage.org/package/warp>.\nextra-source-files:  attic/hex\n                     ChangeLog.md\n                     README.md\n                     test/head-response\n                     test/inputFile\n\nFlag network-bytestring\n    Default: False\n\nFlag allow-sendfilefd\n    Description: Allow use of sendfileFd (not available on GNU/kFreeBSD)\n    Default:     True\n\nFlag warp-debug\n    Description: print debug output. not suitable for production\n    Default:     False\n\nLibrary\n  Build-Depends:     base                      >= 4.10       && < 5\n                   , array\n                   , async\n                   , auto-update               >= 0.1.3    && < 0.2\n                   , bsb-http-chunked                         < 0.1\n                   , bytestring                >= 0.9.1.4\n                   , case-insensitive          >= 0.2\n                   , containers\n                   , ghc-prim\n                   , hashable\n                   , http-date\n                   , http-types                >= 0.12\n                   , http2                     >= 2.0      && < 2.1\n                   , iproute                   >= 1.3.1\n                   , simple-sendfile           >= 0.2.7    && < 0.3\n                   , stm                       >= 2.3\n                   , streaming-commons         >= 0.1.10\n                   , text\n                   , time-manager\n                   , unix-compat               >= 0.2\n                   , vault                     >= 0.3\n                   , wai                       >= 3.2      && < 3.3\n                   , word8\n                   , x509\n  if impl(ghc < 8)\n      Build-Depends: semigroups\n  if flag(network-bytestring)\n      Build-Depends: network                   >= 2.2.1.5  && < 2.2.3\n                   , network-bytestring        >= 0.1.3    && < 0.1.4\n  else\n      Build-Depends: network               >= 2.3\n  Exposed-modules:   Network.Wai.Handler.Warp\n                     Network.Wai.Handler.Warp.Internal\n  Other-modules:     Network.Wai.Handler.Warp.Buffer\n                     Network.Wai.Handler.Warp.Conduit\n                     Network.Wai.Handler.Warp.Counter\n                     Network.Wai.Handler.Warp.Date\n                     Network.Wai.Handler.Warp.FdCache\n                     Network.Wai.Handler.Warp.File\n                     Network.Wai.Handler.Warp.FileInfoCache\n                     Network.Wai.Handler.Warp.HashMap\n                     Network.Wai.Handler.Warp.HTTP1\n                     Network.Wai.Handler.Warp.HTTP2\n                     Network.Wai.Handler.Warp.HTTP2.File\n                     Network.Wai.Handler.Warp.HTTP2.PushPromise\n                     Network.Wai.Handler.Warp.HTTP2.Request\n                     Network.Wai.Handler.Warp.HTTP2.Response\n                     Network.Wai.Handler.Warp.HTTP2.Types\n                     Network.Wai.Handler.Warp.Header\n                     Network.Wai.Handler.Warp.IO\n                     Network.Wai.Handler.Warp.Imports\n                     Network.Wai.Handler.Warp.PackInt\n                     Network.Wai.Handler.Warp.ReadInt\n                     Network.Wai.Handler.Warp.Recv\n                     Network.Wai.Handler.Warp.Request\n                     Network.Wai.Handler.Warp.RequestHeader\n                     Network.Wai.Handler.Warp.Response\n                     Network.Wai.Handler.Warp.ResponseHeader\n                     Network.Wai.Handler.Warp.Run\n                     Network.Wai.Handler.Warp.SendFile\n                     Network.Wai.Handler.Warp.Settings\n                     Network.Wai.Handler.Warp.Types\n                     Network.Wai.Handler.Warp.Windows\n                     Network.Wai.Handler.Warp.WithApplication\n                     Paths_warp\n  Ghc-Options:       -Wall\n\n  if flag(warp-debug)\n      Cpp-Options:   -DWARP_DEBUG\n  if (os(linux) || os(freebsd) || os(darwin)) && flag(allow-sendfilefd)\n      Cpp-Options:   -DSENDFILEFD\n  if os(windows)\n      Cpp-Options:   -DWINDOWS\n      Build-Depends: time\n  else\n      Build-Depends: unix\n      Other-modules: Network.Wai.Handler.Warp.MultiMap\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n  Default-Language:     Haskell2010\n\nTest-Suite doctest\n  buildable:            False\n  Type:                 exitcode-stdio-1.0\n  HS-Source-Dirs:       test\n  Ghc-Options:          -threaded -Wall\n  Main-Is:              doctests.hs\n  Build-Depends:        base >= 4.8 && < 5\n                      , doctest >= 0.10.1\n  if os(windows)\n    Buildable: False\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n  Default-Language:     Haskell2010\n\nTest-Suite spec\n    Main-Is:         Spec.hs\n    Other-modules:   BufferPoolSpec\n                     ConduitSpec\n                     ExceptionSpec\n                     FdCacheSpec\n                     FileSpec\n                     ReadIntSpec\n                     RequestSpec\n                     ResponseHeaderSpec\n                     ResponseSpec\n                     RunSpec\n                     SendFileSpec\n                     WithApplicationSpec\n                     HTTP\n                     Network.Wai.Handler.Warp\n                     Network.Wai.Handler.Warp.Buffer\n                     Network.Wai.Handler.Warp.Conduit\n                     Network.Wai.Handler.Warp.Counter\n                     Network.Wai.Handler.Warp.Date\n                     Network.Wai.Handler.Warp.FdCache\n                     Network.Wai.Handler.Warp.File\n                     Network.Wai.Handler.Warp.FileInfoCache\n                     Network.Wai.Handler.Warp.HTTP1\n                     Network.Wai.Handler.Warp.HTTP2\n                     Network.Wai.Handler.Warp.HTTP2.File\n                     Network.Wai.Handler.Warp.HTTP2.PushPromise\n                     Network.Wai.Handler.Warp.HTTP2.Request\n                     Network.Wai.Handler.Warp.HTTP2.Response\n                     Network.Wai.Handler.Warp.HTTP2.Types\n                     Network.Wai.Handler.Warp.HashMap\n                     Network.Wai.Handler.Warp.Header\n                     Network.Wai.Handler.Warp.IO\n                     Network.Wai.Handler.Warp.Imports\n                     Network.Wai.Handler.Warp.MultiMap\n                     Network.Wai.Handler.Warp.PackInt\n                     Network.Wai.Handler.Warp.ReadInt\n                     Network.Wai.Handler.Warp.Recv\n                     Network.Wai.Handler.Warp.Request\n                     Network.Wai.Handler.Warp.RequestHeader\n                     Network.Wai.Handler.Warp.Response\n                     Network.Wai.Handler.Warp.ResponseHeader\n                     Network.Wai.Handler.Warp.Run\n                     Network.Wai.Handler.Warp.SendFile\n                     Network.Wai.Handler.Warp.Settings\n                     Network.Wai.Handler.Warp.Types\n                     Network.Wai.Handler.Warp.Windows\n                     Network.Wai.Handler.Warp.WithApplication\n                     Paths_warp\n\n    Hs-Source-Dirs:  test, .\n    Type:            exitcode-stdio-1.0\n\n    Ghc-Options:     -Wall -threaded\n    Build-Depends:   base >= 4.8 && < 5\n                   , HUnit\n                   , QuickCheck\n                   , array\n                   , async\n                   , auto-update\n                   , bsb-http-chunked                         < 0.1\n                   , bytestring                >= 0.9.1.4\n                   , case-insensitive          >= 0.2\n                   , containers\n                   , directory\n                   , ghc-prim\n                   , hashable\n                   , hspec                     >= 1.3\n                   , http-client\n                   , http-date\n                   , http-types                >= 0.12\n                   , http2                     >= 2.0      && < 2.1\n                   , iproute                   >= 1.3.1\n                   , lifted-base               >= 0.1\n                   , network\n                   , process\n                   , simple-sendfile           >= 0.2.4    && < 0.3\n                   , stm                       >= 2.3\n                   , streaming-commons         >= 0.1.10\n                   , text\n                   , time\n                   , time-manager\n                   , unix-compat               >= 0.2\n                   , vault\n                   , wai                       >= 3.2      && < 3.3\n                   , word8\n                   , x509\n    -- Build-Tool-Depends: hspec-discover:hspec-discover\n  if impl(ghc < 8)\n      Build-Depends: semigroups\n                   , transformers\n\n  if (os(linux) || os(freebsd) || os(darwin)) && flag(allow-sendfilefd)\n    Cpp-Options:   -DSENDFILEFD\n    Build-Depends: unix\n  if os(windows)\n    Cpp-Options:   -DWINDOWS\n    Build-Depends: time\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n  Default-Language:     Haskell2010\n\nBenchmark parser\n    Type:           exitcode-stdio-1.0\n    Main-Is:        Parser.hs\n    other-modules:  Network.Wai.Handler.Warp.Date\n                    Network.Wai.Handler.Warp.FdCache\n                    Network.Wai.Handler.Warp.FileInfoCache\n                    Network.Wai.Handler.Warp.HashMap\n                    Network.Wai.Handler.Warp.Imports\n                    Network.Wai.Handler.Warp.MultiMap\n                    Network.Wai.Handler.Warp.Types\n    HS-Source-Dirs: bench .\n    Build-Depends:  base >= 4.8 && < 5\n                  , auto-update\n                  , bytestring\n                  , containers\n                  , gauge\n                  , hashable\n                  , http-date\n                  , http-types\n                  , network\n                  , network\n                  , time-manager\n                  , unix-compat\n                  , x509\n  if impl(ghc < 8)\n      Build-Depends: semigroups\n\n  if (os(linux) || os(freebsd) || os(darwin)) && flag(allow-sendfilefd)\n    Cpp-Options:   -DSENDFILEFD\n    Build-Depends: unix\n  if os(windows)\n    Cpp-Options:   -DWINDOWS\n    Build-Depends: time\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n  Default-Language:     Haskell2010\n\nSource-Repository head\n  Type:     git\n  Location: git://github.com/yesodweb/wai.git\n";
    }