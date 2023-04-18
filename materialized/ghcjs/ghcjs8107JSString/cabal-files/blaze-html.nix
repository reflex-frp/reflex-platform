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
      specVersion = "1.8";
      identifier = { name = "blaze-html"; version = "0.9.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jasper Van der Jeugt <m@jaspervdj.be>";
      author = "Jasper Van der Jeugt, Simon Meier";
      homepage = "http://jaspervdj.be/blaze";
      url = "";
      synopsis = "A blazingly fast HTML combinator library for Haskell";
      description = "A blazingly fast HTML combinator library for the Haskell\nprogramming language. The Text.Blaze module is a good\nstarting point, as well as this tutorial:\n<http://jaspervdj.be/blaze/tutorial.html>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "blaze-html-tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/blaze-html-0.9.1.2.tar.gz";
      sha256 = "60503f42546c6c1b954014d188ea137e43d74dcffd2bf6157c113fd91a0c394c";
      });
    }) // {
    package-description-override = "Name:         blaze-html\r\nVersion:      0.9.1.2\r\nx-revision: 1\r\nHomepage:     http://jaspervdj.be/blaze\r\nBug-Reports:  http://github.com/jaspervdj/blaze-html/issues\r\nLicense:      BSD3\r\nLicense-file: LICENSE\r\nAuthor:       Jasper Van der Jeugt, Simon Meier\r\nMaintainer:   Jasper Van der Jeugt <m@jaspervdj.be>\r\nStability:    Experimental\r\nCategory:     Text\r\nSynopsis:     A blazingly fast HTML combinator library for Haskell\r\nDescription:\r\n  A blazingly fast HTML combinator library for the Haskell\r\n  programming language. The Text.Blaze module is a good\r\n  starting point, as well as this tutorial:\r\n  <http://jaspervdj.be/blaze/tutorial.html>.\r\n\r\nBuild-type:    Simple\r\nCabal-version: >= 1.8\r\nTested-with:   GHC == 7.8.4, GHC == 7.10.3,\r\n               GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.1,\r\n               GHC == 8.6.3, GHC == 8.8.1\r\n\r\nExtra-source-files:\r\n  CHANGELOG\r\n  src/Util/Sanitize.hs\r\n  src/Util/GenerateHtmlCombinators.hs\r\n\r\nLibrary\r\n  Hs-source-dirs: src\r\n  Ghc-Options:    -Wall\r\n\r\n  Exposed-modules:\r\n    Text.Blaze.Html\r\n    Text.Blaze.Html.Renderer.Pretty\r\n    Text.Blaze.Html.Renderer.String\r\n    Text.Blaze.Html.Renderer.Text\r\n    Text.Blaze.Html.Renderer.Utf8\r\n    Text.Blaze.Html4.FrameSet\r\n    Text.Blaze.Html4.FrameSet.Attributes\r\n    Text.Blaze.Html4.Strict\r\n    Text.Blaze.Html4.Strict.Attributes\r\n    Text.Blaze.Html4.Transitional\r\n    Text.Blaze.Html4.Transitional.Attributes\r\n    Text.Blaze.Html5\r\n    Text.Blaze.Html5.Attributes\r\n    Text.Blaze.XHtml1.FrameSet\r\n    Text.Blaze.XHtml1.FrameSet.Attributes\r\n    Text.Blaze.XHtml1.Strict\r\n    Text.Blaze.XHtml1.Strict.Attributes\r\n    Text.Blaze.XHtml1.Transitional\r\n    Text.Blaze.XHtml1.Transitional.Attributes\r\n    Text.Blaze.XHtml5\r\n    Text.Blaze.XHtml5.Attributes\r\n\r\n  Build-depends:\r\n    base          >= 4    && < 5,\r\n    blaze-builder >= 0.3  && < 0.5,\r\n    blaze-markup  >= 0.8  && < 0.9,\r\n    bytestring    >= 0.9  && < 0.12,\r\n    text          >= 0.10 && < 1.3\r\n\r\nTest-suite blaze-html-tests\r\n  Type:           exitcode-stdio-1.0\r\n  Hs-source-dirs: src tests\r\n  Main-is:        TestSuite.hs\r\n  Ghc-options:    -Wall\r\n\r\n  Other-modules:\r\n    Text.Blaze.Html\r\n    Text.Blaze.Html.Renderer.String\r\n    Text.Blaze.Html.Renderer.Text\r\n    Text.Blaze.Html.Renderer.Utf8\r\n    Text.Blaze.Html.Tests\r\n    Text.Blaze.Html.Tests.Util\r\n    Text.Blaze.Html5\r\n    Text.Blaze.Html5.Attributes\r\n    Util.Sanitize\r\n    Util.Tests\r\n\r\n  Build-depends:\r\n    HUnit                      >= 1.2 && < 1.7,\r\n    QuickCheck                 >= 2.4 && < 2.14,\r\n    containers                 >= 0.3 && < 0.7,\r\n    test-framework             >= 0.4 && < 0.9,\r\n    test-framework-hunit       >= 0.3 && < 0.4,\r\n    test-framework-quickcheck2 >= 0.3 && < 0.4,\r\n    -- Copied from regular dependencies...\r\n    base          >= 4    && < 5,\r\n    blaze-builder >= 0.3  && < 0.5,\r\n    blaze-markup  >= 0.8  && < 0.9,\r\n    bytestring    >= 0.9  && < 0.12,\r\n    text          >= 0.10 && < 1.3\r\n\r\nSource-repository head\r\n  Type:     git\r\n  Location: http://github.com/jaspervdj/blaze-html.git\r\n";
    }