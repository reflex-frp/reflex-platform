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
      warn-as-error = false;
      conduit10 = false;
      warp-tests = false;
      network-uri = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "HTTP"; version = "4000.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Ganesh Sittampalam <ganesh@earth.li>";
      author = "Warrick Gray <warrick.gray@hotmail.com>";
      homepage = "https://github.com/haskell/HTTP";
      url = "";
      synopsis = "A library for client-side HTTP";
      description = "The HTTP package supports client-side web programming in Haskell. It lets you set up\nHTTP connections, transmitting requests and processing the responses coming back, all\nfrom within the comforts of Haskell. It's dependent on the network package to operate,\nbut other than that, the implementation is all written in Haskell.\n\nA basic API for issuing single HTTP requests + receiving responses is provided. On top\nof that, a session-level abstraction is also on offer  (the @BrowserAction@ monad);\nit taking care of handling the management of persistent connections, proxies,\nstate (cookies) and authentication credentials required to handle multi-step\ninteractions with a web server.\n\nThe representation of the bytes flowing across is extensible via the use of a type class,\nletting you pick the representation of requests and responses that best fits your use.\nSome pre-packaged, common instances are provided for you (@ByteString@, @String@).\n\nHere's an example use:\n\n>\n>    do\n>      rsp <- Network.HTTP.simpleHTTP (getRequest \"http://www.haskell.org/\")\n>              -- fetch document and return it (as a 'String'.)\n>      fmap (take 100) (getResponseBody rsp)\n>\n>    do\n>      (_, rsp)\n>         <- Network.Browser.browse $ do\n>               setAllowRedirects True -- handle HTTP redirects\n>               request $ getRequest \"http://www.haskell.org/\"\n>      return (take 100 (rspBody rsp))\n\n__Note:__ This package does not support HTTPS connections.\nIf you need HTTPS, take a look at the following packages:\n\n* <http://hackage.haskell.org/package/http-streams http-streams>\n\n* <http://hackage.haskell.org/package/http-client http-client> (in combination with\n<http://hackage.haskell.org/package/http-client-tls http-client-tls>)\n\n* <http://hackage.haskell.org/package/req req>\n\n* <http://hackage.haskell.org/package/wreq wreq>\n";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ] ++ (if flags.network-uri
          then [
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ])) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = ([
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."httpd-shed" or (errorHandler.buildDepError "httpd-shed"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."pureMD5" or (errorHandler.buildDepError "pureMD5"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ] ++ (if flags.network-uri
            then [
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
              ]
            else [
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
              ])) ++ (pkgs.lib).optionals (flags.warp-tests) ([
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ] ++ (if flags.conduit10
            then [
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              ]
            else [
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
              ]));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/HTTP-4000.4.1.tar.gz";
      sha256 = "df31d8efec775124dab856d7177ddcba31be9f9e0836ebdab03d94392f2dd453";
      });
    }) // {
    package-description-override = "Cabal-Version: >= 1.10\nName: HTTP\nVersion: 4000.4.1\nx-revision: 1\nBuild-type: Simple\nLicense: BSD3\nLicense-file: LICENSE\nAuthor: Warrick Gray <warrick.gray@hotmail.com>\nMaintainer: Ganesh Sittampalam <ganesh@earth.li>\nHomepage: https://github.com/haskell/HTTP\nCategory: Network\nSynopsis: A library for client-side HTTP\nDescription:\n\n The HTTP package supports client-side web programming in Haskell. It lets you set up\n HTTP connections, transmitting requests and processing the responses coming back, all\n from within the comforts of Haskell. It's dependent on the network package to operate,\n but other than that, the implementation is all written in Haskell.\n .\n A basic API for issuing single HTTP requests + receiving responses is provided. On top\n of that, a session-level abstraction is also on offer  (the @BrowserAction@ monad);\n it taking care of handling the management of persistent connections, proxies,\n state (cookies) and authentication credentials required to handle multi-step\n interactions with a web server.\n .\n The representation of the bytes flowing across is extensible via the use of a type class,\n letting you pick the representation of requests and responses that best fits your use.\n Some pre-packaged, common instances are provided for you (@ByteString@, @String@).\n .\n Here's an example use:\n .\n >\n >    do\n >      rsp <- Network.HTTP.simpleHTTP (getRequest \"http://www.haskell.org/\")\n >              -- fetch document and return it (as a 'String'.)\n >      fmap (take 100) (getResponseBody rsp)\n >\n >    do\n >      (_, rsp)\n >         <- Network.Browser.browse $ do\n >               setAllowRedirects True -- handle HTTP redirects\n >               request $ getRequest \"http://www.haskell.org/\"\n >      return (take 100 (rspBody rsp))\n .\n __Note:__ This package does not support HTTPS connections.\n If you need HTTPS, take a look at the following packages:\n .\n * <http://hackage.haskell.org/package/http-streams http-streams>\n .\n * <http://hackage.haskell.org/package/http-client http-client> (in combination with\n <http://hackage.haskell.org/package/http-client-tls http-client-tls>)\n .\n * <http://hackage.haskell.org/package/req req>\n .\n * <http://hackage.haskell.org/package/wreq wreq>\n .\n\nExtra-Source-Files: CHANGES\n\ntested-with:\n  GHC==9.2.1, GHC==9.0.1,\n  GHC==8.10.7, GHC==8.8.4, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2,\n  GHC==7.10.3, GHC==7.8.4, GHC==7.6.3\n\nSource-Repository head\n  type: git\n  location: https://github.com/haskell/HTTP.git\n\nFlag warn-as-error\n  default:     False\n  description: Build with warnings-as-errors\n  manual:      True\n\nFlag conduit10\n  description: Use version 1.0.x or below of the conduit package (for the test suite)\n  default: False\n\nFlag warp-tests\n  description: Test against warp\n  default:     False\n  manual:      True\n\nflag network-uri\n  description: Get Network.URI from the network-uri package\n  default: True\n\nLibrary\n  Exposed-modules:\n                 Network.BufferType,\n                 Network.Stream,\n                 Network.StreamDebugger,\n                 Network.StreamSocket,\n                 Network.TCP,\n                 Network.HTTP,\n                 Network.HTTP.Headers,\n                 Network.HTTP.Base,\n                 Network.HTTP.Stream,\n                 Network.HTTP.Auth,\n                 Network.HTTP.Cookie,\n                 Network.HTTP.Proxy,\n                 Network.HTTP.HandleStream,\n                 Network.Browser\n  Other-modules:\n                 Network.HTTP.Base64,\n                 Network.HTTP.MD5Aux,\n                 Network.HTTP.Utils\n                 Paths_HTTP\n  GHC-options: -fwarn-missing-signatures -Wall\n\n  -- note the test harness constraints should be kept in sync with these\n  -- where dependencies are shared\n  build-depends:\n      base          >= 4.6.0.0   && < 4.18\n    , array         >= 0.3.0.2   && < 0.6\n    , bytestring    >= 0.9.1.5   && < 0.12\n    , parsec        >= 2.0       && < 3.2\n    , time          >= 1.1.2.3   && < 1.13\n    , transformers  >= 0.2.0.0   && < 0.7\n        -- transformers-0.2.0.0 is the first to have Control.Monad.IO.Class\n    -- The following dependencies are refined by flags, but they should\n    -- still be mentioned here on the top-level.\n    , mtl           >= 2.0.0.0   && < 2.4\n    , network       >= 2.4       && < 3.2\n\n  default-language: Haskell98\n  default-extensions: FlexibleInstances\n\n  if flag(network-uri)\n    Build-depends: network-uri == 2.6.*, network >= 2.6\n  else\n    Build-depends: network < 2.6\n\n  if flag(warn-as-error)\n    ghc-options:      -Werror\n\n  if os(windows)\n    Build-depends: Win32 >= 2.2.0.0 && < 2.14\n\nTest-Suite test\n  type: exitcode-stdio-1.0\n\n  default-language: Haskell98\n  hs-source-dirs: test\n  main-is: httpTests.hs\n\n  other-modules:\n    Httpd\n    UnitTests\n\n  ghc-options: -Wall\n\n  build-depends:\n      HTTP\n    -- constraints inherited from HTTP\n    , base\n    , bytestring\n    , mtl\n    , network\n    -- extra dependencies\n    , deepseq               >= 1.3.0.0  && < 1.5\n    , httpd-shed            >= 0.4      && < 0.5\n    , HUnit                 >= 1.2.0.1  && < 1.7\n    , pureMD5               >= 0.2.4    && < 2.2\n    , split                 >= 0.1.3    && < 0.3\n    , test-framework        >= 0.2.0    && < 0.9\n    , test-framework-hunit  >= 0.3.0    && < 0.4\n\n  if flag(network-uri)\n    Build-depends: network-uri == 2.6.*, network >= 2.6\n  else\n    Build-depends: network < 2.6\n\n  if flag(warp-tests)\n    CPP-Options: -DWARP_TESTS\n    build-depends:\n        case-insensitive    >= 0.4.0.1  && < 1.3\n      , conduit             >= 1.0.8    && < 1.4\n      , http-types          >= 0.8.0    && < 1.0\n      , wai                 >= 2.1.0    && < 3.3\n      , warp                >= 2.1.0    && < 3.4\n\n    if flag(conduit10)\n      build-depends: conduit < 1.1\n    else\n      build-depends: conduit >= 1.1, conduit-extra >= 1.1 && < 1.4\n";
    }