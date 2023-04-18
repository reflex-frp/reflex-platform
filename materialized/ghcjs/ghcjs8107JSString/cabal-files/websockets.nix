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
    flags = { example = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "websockets"; version = "0.12.7.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2010-2011 Siniša Biđin\n(c) 2011-2018 Jasper Van der Jeugt\n(c) 2011 Steffen Schuldenzucker\n(c) 2011 Alex Lang";
      maintainer = "Jasper Van der Jeugt <m@jaspervdj.be>";
      author = "Siniša Biđin <sinisa@bidin.cc>\nJasper Van der Jeugt <m@jaspervdj.be>\nSteffen Schuldenzucker <steffen.schuldenzucker@googlemail.com>\nAlex Lang <lang@tsurucapital.com>";
      homepage = "http://jaspervdj.be/websockets";
      url = "";
      synopsis = "A sensible and clean way to write WebSocket-capable servers in Haskell.";
      description = "This library allows you to write WebSocket-capable servers.\n\nAn example server:\n<https://github.com/jaspervdj/websockets/blob/master/example/server.lhs>\n\nAn example client:\n<https://github.com/jaspervdj/websockets/blob/master/example/client.hs>\n\nSee also:\n\n* The specification of the WebSocket protocol:\n<http://www.whatwg.org/specs/web-socket-protocol/>\n\n* The JavaScript API for dealing with WebSockets:\n<http://www.w3.org/TR/websockets/>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
          ];
        buildable = true;
        };
      exes = {
        "websockets-example" = {
          depends = [
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            ];
          buildable = if !flags.example then false else true;
          };
        "websockets-autobahn" = {
          depends = [
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            ];
          buildable = if !flags.example then false else true;
          };
        };
      tests = {
        "websockets-tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-mask" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/websockets-0.12.7.2.tar.gz";
      sha256 = "84c45a5db481b4c969dddfa7d3cca257ac2a97801594f1180b596d41035122ad";
      });
    }) // {
    package-description-override = "Name:    websockets\nVersion: 0.12.7.2\n\nSynopsis:\n  A sensible and clean way to write WebSocket-capable servers in Haskell.\n\nDescription:\n This library allows you to write WebSocket-capable servers.\n .\n An example server:\n <https://github.com/jaspervdj/websockets/blob/master/example/server.lhs>\n .\n An example client:\n <https://github.com/jaspervdj/websockets/blob/master/example/client.hs>\n .\n See also:\n .\n * The specification of the WebSocket protocol:\n <http://www.whatwg.org/specs/web-socket-protocol/>\n .\n * The JavaScript API for dealing with WebSockets:\n <http://www.w3.org/TR/websockets/>\n\nLicense:       BSD3\nLicense-file:  LICENCE\nCopyright:     (c) 2010-2011 Siniša Biđin\n               (c) 2011-2018 Jasper Van der Jeugt\n               (c) 2011 Steffen Schuldenzucker\n               (c) 2011 Alex Lang\nAuthor:        Siniša Biđin <sinisa@bidin.cc>\n               Jasper Van der Jeugt <m@jaspervdj.be>\n               Steffen Schuldenzucker <steffen.schuldenzucker@googlemail.com>\n               Alex Lang <lang@tsurucapital.com>\nMaintainer:    Jasper Van der Jeugt <m@jaspervdj.be>\nStability:     experimental\nCategory:      Network\nBuild-type:    Simple\nCabal-version: >= 1.10\n\nHomepage:    http://jaspervdj.be/websockets\nBug-reports: https://github.com/jaspervdj/websockets/issues\n\nExtra-source-files:\n  CHANGELOG\n\nSource-repository head\n  Type:     git\n  Location: https://github.com/jaspervdj/websockets\n\nFlag Example\n  Description: Build the example server\n  Default:     False\n  Manual:      True\n\nLibrary\n  Hs-source-dirs: src\n  Ghc-options:      -Wall\n  C-sources:        cbits/cbits.c\n  Default-language: Haskell2010\n\n  Exposed-modules:\n    Network.WebSockets\n    Network.WebSockets.Client\n    Network.WebSockets.Connection\n    Network.WebSockets.Extensions\n    Network.WebSockets.Stream\n    -- Network.WebSockets.Util.PubSub TODO\n\n  Other-modules:\n    Network.WebSockets.Connection.Options\n    Network.WebSockets.Extensions.Description\n    Network.WebSockets.Extensions.PermessageDeflate\n    Network.WebSockets.Extensions.StrictUnicode\n    Network.WebSockets.Http\n    Network.WebSockets.Hybi13\n    Network.WebSockets.Hybi13.Demultiplex\n    Network.WebSockets.Hybi13.Mask\n    Network.WebSockets.Protocol\n    Network.WebSockets.Server\n    Network.WebSockets.Types\n\n  Build-depends:\n    async             >= 2.2    && < 2.3,\n    attoparsec        >= 0.10   && < 0.14,\n    base              >= 4.8    && < 5,\n    base64-bytestring >= 0.1    && < 1.3,\n    binary            >= 0.8.1  && < 0.11,\n    bytestring        >= 0.9    && < 0.12,\n    bytestring-builder             < 0.11,\n    case-insensitive  >= 0.3    && < 1.3,\n    clock             >= 0.8    && < 0.9,\n    containers        >= 0.3    && < 0.7,\n    network           >= 2.3    && < 3.2,\n    random            >= 1.0    && < 1.3,\n    SHA               >= 1.5    && < 1.7,\n    streaming-commons >= 0.1    && < 0.3,\n    text              >= 0.10   && < 1.3,\n    entropy           >= 0.2.1  && < 0.5\n\nTest-suite websockets-tests\n  Type:             exitcode-stdio-1.0\n  Hs-source-dirs:   src tests/haskell\n  Main-is:          TestSuite.hs\n  Ghc-options:      -Wall\n  C-sources:        cbits/cbits.c\n  Default-language: Haskell2010\n\n  Other-modules:\n    Network.WebSockets\n    Network.WebSockets.Client\n    Network.WebSockets.Connection\n    Network.WebSockets.Connection.Options\n    Network.WebSockets.Extensions\n    Network.WebSockets.Extensions.Description\n    Network.WebSockets.Extensions.PermessageDeflate\n    Network.WebSockets.Extensions.PermessageDeflate.Tests\n    Network.WebSockets.Extensions.StrictUnicode\n    Network.WebSockets.Extensions.Tests\n    Network.WebSockets.Handshake.Tests\n    Network.WebSockets.Http\n    Network.WebSockets.Http.Tests\n    Network.WebSockets.Hybi13\n    Network.WebSockets.Hybi13.Demultiplex\n    Network.WebSockets.Hybi13.Demultiplex.Tests\n    Network.WebSockets.Hybi13.Mask\n    Network.WebSockets.Mask.Tests\n    Network.WebSockets.Protocol\n    Network.WebSockets.Server\n    Network.WebSockets.Server.Tests\n    Network.WebSockets.Stream\n    Network.WebSockets.Tests\n    Network.WebSockets.Tests.Util\n    Network.WebSockets.Types\n    Paths_websockets\n\n  Build-depends:\n    HUnit                      >= 1.2 && < 1.7,\n    QuickCheck                 >= 2.7 && < 2.15,\n    test-framework             >= 0.4 && < 0.9,\n    test-framework-hunit       >= 0.2 && < 0.4,\n    test-framework-quickcheck2 >= 0.2 && < 0.4,\n    -- Copied from regular dependencies...\n    async             >= 2.2    && < 2.3,\n    attoparsec        >= 0.10   && < 0.14,\n    base              >= 4      && < 5,\n    base64-bytestring >= 0.1    && < 1.3,\n    binary            >= 0.8.1  && < 0.11,\n    bytestring        >= 0.9    && < 0.12,\n    bytestring-builder             < 0.11,\n    case-insensitive  >= 0.3    && < 1.3,\n    clock             >= 0.8    && < 0.9,\n    containers        >= 0.3    && < 0.7,\n    network           >= 2.3    && < 3.2,\n    random            >= 1.0    && < 1.3,\n    SHA               >= 1.5    && < 1.7,\n    streaming-commons >= 0.1    && < 0.3,\n    text              >= 0.10   && < 1.3,\n    entropy           >= 0.2.1  && < 0.5\n\nExecutable websockets-example\n  If !flag(Example)\n    Buildable: False\n\n  Hs-source-dirs:   example\n  Main-is:          server.lhs\n  Ghc-options:      -Wall\n  Default-language: Haskell2010\n\n  Build-depends:\n    websockets,\n    -- Copied from regular dependencies...\n    async             >= 2.2    && < 2.3,\n    attoparsec        >= 0.10   && < 0.14,\n    base              >= 4      && < 5,\n    base64-bytestring >= 0.1    && < 1.3,\n    binary            >= 0.8.1  && < 0.11,\n    bytestring        >= 0.9    && < 0.12,\n    bytestring-builder             < 0.11,\n    case-insensitive  >= 0.3    && < 1.3,\n    clock             >= 0.8    && < 0.9,\n    containers        >= 0.3    && < 0.7,\n    network           >= 2.3    && < 3.2,\n    random            >= 1.0    && < 1.3,\n    SHA               >= 1.5    && < 1.7,\n    text              >= 0.10   && < 1.3,\n    entropy           >= 0.2.1  && < 0.5\n\nExecutable websockets-autobahn\n  If !flag(Example)\n    Buildable: False\n\n  Hs-source-dirs:   tests/autobahn\n  Main-is:          server.hs\n  Ghc-options:      -Wall -threaded -O2 -rtsopts \"-with-rtsopts=-N\"\n  Default-language: Haskell2010\n\n  Other-modules:\n    Paths_websockets\n\n  Build-depends:\n    websockets,\n    -- Copied from regular dependencies...\n    async             >= 2.2    && < 2.3,\n    attoparsec        >= 0.10   && < 0.14,\n    base              >= 4      && < 5,\n    base64-bytestring >= 0.1    && < 1.3,\n    binary            >= 0.8.1  && < 0.11,\n    bytestring        >= 0.9    && < 0.12,\n    bytestring-builder             < 0.11,\n    case-insensitive  >= 0.3    && < 1.3,\n    clock             >= 0.8    && < 0.9,\n    containers        >= 0.3    && < 0.7,\n    network           >= 2.3    && < 3.2,\n    random            >= 1.0    && < 1.3,\n    SHA               >= 1.5    && < 1.7,\n    text              >= 0.10   && < 1.3,\n    entropy           >= 0.2.1  && < 0.5\n\nBenchmark bench-mask\n  Type:             exitcode-stdio-1.0\n  Main-is:          mask.hs\n  C-sources:        cbits/cbits.c\n  Hs-source-dirs:   benchmarks, src\n  Default-language: Haskell2010\n\n  Other-modules:\n    Network.WebSockets.Hybi13.Mask\n\n  Build-depends:\n    criterion,\n    -- Copied from regular dependencies...\n    async             >= 2.2    && < 2.3,\n    attoparsec        >= 0.10   && < 0.14,\n    base              >= 4      && < 5,\n    base64-bytestring >= 0.1    && < 1.3,\n    binary            >= 0.8.1  && < 0.11,\n    bytestring        >= 0.9    && < 0.12,\n    bytestring-builder             < 0.11,\n    case-insensitive  >= 0.3    && < 1.3,\n    clock             >= 0.8    && < 0.9,\n    containers        >= 0.3    && < 0.7,\n    network           >= 2.3    && < 3.2,\n    random            >= 1.0    && < 1.3,\n    SHA               >= 1.5    && < 1.7,\n    text              >= 0.10   && < 1.3,\n    entropy           >= 0.2.1  && < 0.5\n";
    }