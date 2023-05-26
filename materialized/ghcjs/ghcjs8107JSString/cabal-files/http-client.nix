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
    flags = { network-uri = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "http-client"; version = "0.7.6"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/snoyberg/http-client";
      url = "";
      synopsis = "An HTTP client engine";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/http-client>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          ] ++ (if flags.network-uri
          then [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ])) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."unsupported-ghc-version" or (errorHandler.buildDepError "unsupported-ghc-version"))) ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            ];
          buildable = true;
          };
        "spec-nonet" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/http-client-0.7.6.tar.gz";
      sha256 = "33f378976118f9d800fa526452ada06314c3b4f9eab134e1a4d215380baea890";
      });
    }) // {
    package-description-override = "name:                http-client\nversion:             0.7.6\nsynopsis:            An HTTP client engine\ndescription:         Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/http-client>.\nhomepage:            https://github.com/snoyberg/http-client\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Michael Snoyman\nmaintainer:          michael@snoyman.com\ncategory:            Network\nbuild-type:          Simple\nextra-source-files:  README.md ChangeLog.md\ncabal-version:       >=1.10\n\nflag network-uri\n   description: Get Network.URI from the network-uri package\n   default: True\n\nlibrary\n  hs-source-dirs:      ., publicsuffixlist\n  exposed-modules:     Network.HTTP.Client\n                       Network.HTTP.Client.MultipartFormData\n                       Network.HTTP.Client.Internal\n  other-modules:       Network.HTTP.Client.Body\n                       Network.HTTP.Client.Connection\n                       Network.HTTP.Client.Cookies\n                       Network.HTTP.Client.Core\n                       Network.HTTP.Client.Headers\n                       Network.HTTP.Client.Manager\n                       Network.HTTP.Client.Request\n                       Network.HTTP.Client.Response\n                       Network.HTTP.Client.Types\n                       Network.HTTP.Client.Util\n                       Network.HTTP.Proxy\n                       Network.PublicSuffixList.Lookup\n                       Network.PublicSuffixList.Types\n                       Network.PublicSuffixList.Serialize\n                       Network.PublicSuffixList.DataStructure\n                       Data.KeyedPool\n  build-depends:       base              >= 4.10   && < 5\n                     , bytestring        >= 0.10\n                     , text              >= 0.11\n                     , http-types        >= 0.8\n                     , blaze-builder     >= 0.3\n                     , time              >= 1.2\n                     , network           >= 2.4\n                     , streaming-commons >= 0.1.0.2 && < 0.3\n                     , containers        >= 0.5\n                     , transformers\n                     , deepseq           >= 1.3    && <1.5\n                     , case-insensitive  >= 1.0\n                     , base64-bytestring >= 1.0\n                     , cookie\n                     , exceptions        >= 0.4\n                     , array\n                     , random\n                     , filepath\n                     , mime-types\n                     , ghc-prim\n                     , stm               >= 2.3\n  if flag(network-uri)\n    build-depends: network >= 2.6, network-uri >= 2.6\n  else\n    build-depends: network < 2.6\n\n  if !impl(ghc>=8.0)\n    build-depends: semigroups >= 0.16.1\n\n  -- See build failure at https://travis-ci.org/snoyberg/http-client/jobs/359573631\n  if impl(ghc < 7.10)\n    -- Disable building with GHC before 8.0.2.\n    -- Due to a cabal bug, do not use buildable: False,\n    -- but instead give it an impossible constraint.\n    -- See: https://github.com/haskell-infra/hackage-trustees/issues/165\n    build-depends: unsupported-ghc-version > 1 && < 1\n\n\n  if os(mingw32)\n    build-depends: Win32, safe\n\n  default-language:    Haskell2010\n\ntest-suite spec\n  main-is:             Spec.hs\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  default-language:    Haskell2010\n  other-modules:       Network.HTTP.ClientSpec\n  build-depends:       base\n                     , http-client\n                     , hspec\n                     , monad-control\n                     , bytestring\n                     , text\n                     , http-types\n                     , blaze-builder\n                     , time\n                     , network\n                     , containers\n                     , transformers\n                     , deepseq\n                     , case-insensitive\n                     , zlib\n                     , async\n                     , streaming-commons >= 0.1.1\n\n\ntest-suite spec-nonet\n  main-is:             Spec.hs\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test-nonet\n  default-language:    Haskell2010\n  ghc-options:         -threaded\n  if os(windows)\n    cpp-options:       -DWINDOWS\n  other-modules:       Network.HTTP.ClientSpec\n                       Network.HTTP.Client.ResponseSpec\n                       Network.HTTP.Client.BodySpec\n                       Network.HTTP.Client.HeadersSpec\n                       Network.HTTP.Client.RequestSpec\n                       Network.HTTP.Client.RequestBodySpec\n                       Network.HTTP.Client.CookieSpec\n  build-depends:       base\n                     , http-client\n                     , hspec\n                     , monad-control\n                     , bytestring\n                     , cookie\n                     , text\n                     , http-types\n                     , blaze-builder\n                     , time\n                     , network\n                     , network-uri\n                     , containers\n                     , transformers\n                     , deepseq\n                     , case-insensitive\n                     , zlib\n                     , async\n                     , streaming-commons >= 0.1.1\n                     , directory\n";
    }