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
    flags = { build-example = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "wai-extra"; version = "3.0.32"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "Provides some basic WAI handlers and middleware.";
      description = "Provides basic WAI handler and middleware functionality:\n\n* WAI Testing Framework\n\nHspec testing facilities and helpers for WAI.\n\n* Event Source/Event Stream\n\nSend server events to the client. Compatible with the JavaScript\nEventSource API.\n\n* Accept Override\n\nOverride the Accept header in a request. Special handling for the\n_accept query parameter (which is used throughout WAI override the\nAccept header).\n\n* Add Headers\n\nWAI Middleware for adding arbitrary headers to an HTTP request.\n\n* Clean Path\n\nClean a request path to a canonical form.\n\n* GZip Compression\n\nNegotiate HTTP payload gzip compression.\n\n* HTTP Basic Authentication\n\nWAI Basic Authentication Middleware which uses Authorization header.\n\n* JSONP\n\n\\\"JSON with Padding\\\" middleware. Automatic wrapping of JSON\nresponses to convert into JSONP.\n\n* Method Override / Post\n\nAllows overriding of the HTTP request method via the _method query string\nparameter.\n\n* Request Logging\n\nRequest logging middleware for development and production environments\n\n* Request Rewrite\n\nRewrite request path info based on a custom conversion rules.\n\n* Stream Files\n\nConvert ResponseFile type responses into ResponseStream type.\n\n* Virtual Host\n\nRedirect incoming requests to a new host based on custom rules.\n\n\nAPI docs and the README are available at <http://www.stackage.org/package/wai-extra>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
          (hsPkgs."wai-logger" or (errorHandler.buildDepError "wai-logger"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      exes = {
        "example" = {
          depends = (pkgs.lib).optionals (flags.build-example) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ];
          buildable = if flags.build-example then true else false;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wai-extra-3.0.32.tar.gz";
      sha256 = "0052878f765dda7a8cbd2c5b72295a80d0251a03b267dbb67633d3aafcaee698";
      });
    }) // {
    package-description-override = "Name:                wai-extra\nVersion:             3.0.32\nSynopsis:            Provides some basic WAI handlers and middleware.\ndescription:\n  Provides basic WAI handler and middleware functionality:\n  .\n  * WAI Testing Framework\n  .\n  Hspec testing facilities and helpers for WAI.\n  .\n  * Event Source/Event Stream\n  .\n  Send server events to the client. Compatible with the JavaScript\n  EventSource API.\n  .\n  * Accept Override\n  .\n  Override the Accept header in a request. Special handling for the\n  _accept query parameter (which is used throughout WAI override the\n  Accept header).\n  .\n  * Add Headers\n  .\n  WAI Middleware for adding arbitrary headers to an HTTP request.\n  .\n  * Clean Path\n  .\n  Clean a request path to a canonical form.\n  .\n  * GZip Compression\n  .\n  Negotiate HTTP payload gzip compression.\n  .\n  * HTTP Basic Authentication\n  .\n  WAI Basic Authentication Middleware which uses Authorization header.\n  .\n  * JSONP\n  .\n  \\\"JSON with Padding\\\" middleware. Automatic wrapping of JSON\n  responses to convert into JSONP.\n  .\n  * Method Override / Post\n  .\n  Allows overriding of the HTTP request method via the _method query string\n  parameter.\n  .\n  * Request Logging\n  .\n  Request logging middleware for development and production environments\n  .\n  * Request Rewrite\n  .\n  Rewrite request path info based on a custom conversion rules.\n  .\n  * Stream Files\n  .\n  Convert ResponseFile type responses into ResponseStream type.\n  .\n  * Virtual Host\n  .\n  Redirect incoming requests to a new host based on custom rules.\n  .\n  .\n  API docs and the README are available at <http://www.stackage.org/package/wai-extra>.\n\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman\nMaintainer:          michael@snoyman.com\nHomepage:            http://github.com/yesodweb/wai\nCategory:            Web\nBuild-Type:          Simple\nCabal-Version:       >=1.10\nStability:           Stable\nextra-source-files:\n  test/requests/dalvik-request\n  test/json\n  test/test.html\n  test/sample.hs\n  ChangeLog.md\n  README.md\n\nflag build-example\n  description:        Build example executable.\n  manual:             True\n  default:            False\n\nLibrary\n  Build-Depends:     base                      >= 4.8 && < 5\n                   , bytestring                >= 0.10.4\n                   , wai                       >= 3.0.3.0  && < 3.3\n                   , old-locale                >= 1.0.0.2  && < 1.1\n                   , time                      >= 1.1.4\n                   , network                   >= 2.6.1.0\n                   , directory                 >= 1.0.1\n                   , transformers              >= 0.2.2\n                   , http-types                >= 0.7\n                   , text                      >= 0.7\n                   , case-insensitive          >= 0.2\n                   , data-default-class\n                   , fast-logger               >= 2.4.5\n                   , wai-logger                >= 2.3.2\n                   , ansi-terminal\n                   , resourcet                 >= 0.4.6    && < 1.3\n                   , void                      >= 0.5\n                   , containers\n                   , base64-bytestring\n                   , word8\n                   , deepseq\n                   , streaming-commons         >= 0.2\n                   , unix-compat\n                   , cookie\n                   , vault\n                   , zlib\n                   , aeson\n                   , iproute\n                   , http2\n\n  if os(windows)\n      cpp-options:   -DWINDOWS\n  else\n      build-depends: unix\n\n  default-extensions:        OverloadedStrings\n\n  Exposed-modules:   Network.Wai.Handler.CGI\n                     Network.Wai.Handler.SCGI\n                     Network.Wai.Header\n                     Network.Wai.Middleware.AcceptOverride\n                     Network.Wai.Middleware.AddHeaders\n                     Network.Wai.Middleware.Approot\n                     Network.Wai.Middleware.Autohead\n                     Network.Wai.Middleware.CleanPath\n                     Network.Wai.Middleware.Local\n                     Network.Wai.Middleware.RequestLogger\n                     Network.Wai.Middleware.RequestLogger.JSON\n                     Network.Wai.Middleware.Gzip\n                     Network.Wai.Middleware.Jsonp\n                     Network.Wai.Middleware.MethodOverride\n                     Network.Wai.Middleware.MethodOverridePost\n                     Network.Wai.Middleware.Rewrite\n                     Network.Wai.Middleware.StripHeaders\n                     Network.Wai.Middleware.Vhost\n                     Network.Wai.Middleware.HttpAuth\n                     Network.Wai.Middleware.StreamFile\n                     Network.Wai.Middleware.ForceDomain\n                     Network.Wai.Middleware.ForceSSL\n                     Network.Wai.Middleware.Routed\n                     Network.Wai.Middleware.Timeout\n                     Network.Wai.Parse\n                     Network.Wai.Request\n                     Network.Wai.UrlMap\n                     Network.Wai.Test\n                     Network.Wai.Test.Internal\n                     Network.Wai.EventSource\n                     Network.Wai.EventSource.EventStream\n  other-modules:     Network.Wai.Middleware.RequestLogger.Internal\n  default-language:          Haskell2010\n  ghc-options:       -Wall\n\nexecutable example\n  hs-source-dirs:      example\n  main-is:             Main.hs\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall\n  if flag(build-example)\n    build-depends:     base\n                     , wai-extra\n                     , warp\n                     , wai\n                     , time\n                     , http-types\n                     , bytestring\n  else\n    buildable: False\n  default-language:    Haskell2010\n\ntest-suite spec\n    type:            exitcode-stdio-1.0\n    hs-source-dirs:  test\n    main-is:         Spec.hs\n    other-modules:   Network.Wai.TestSpec\n                     Network.Wai.ParseSpec\n                     Network.Wai.RequestSpec\n                     Network.Wai.Middleware.ApprootSpec\n                     Network.Wai.Middleware.ForceSSLSpec\n                     Network.Wai.Middleware.RoutedSpec\n                     Network.Wai.Middleware.StripHeadersSpec\n                     Network.Wai.Middleware.TimeoutSpec\n                     WaiExtraSpec\n    build-depends:   base                      >= 4        && < 5\n                   , wai-extra\n                   , wai\n                   , hspec >= 1.3\n                   , transformers\n                   , fast-logger\n                   , http-types\n                   , zlib\n                   , text\n                   , resourcet\n                   , bytestring\n                   , HUnit\n                   , cookie\n                   , time\n                   , case-insensitive\n                   , http2\n    ghc-options:     -Wall\n    default-language:          Haskell2010\n\nsource-repository head\n  type:     git\n  location: git://github.com/yesodweb/wai.git\n";
    }