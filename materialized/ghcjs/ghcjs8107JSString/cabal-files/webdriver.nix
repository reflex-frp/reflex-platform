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
    flags = { network-uri = true; developer = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "webdriver"; version = "0.9.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "kallisti.dev@gmail.com";
      author = "Adam Curtis";
      homepage = "https://github.com/kallisti-dev/hs-webdriver";
      url = "";
      synopsis = "a Haskell client for the Selenium WebDriver protocol";
      description = "A Selenium WebDriver client for Haskell.\nYou can use it to automate browser sessions\nfor testing, system administration, etc.\n\nFor more information about Selenium itself, see\n<http://seleniumhq.org/>\n\nTo find out what's been changed in this version and others,\nsee the change log at\n<https://github.com/kallisti-dev/hs-webdriver/blob/master/CHANGELOG.md>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."zip-archive" or (errorHandler.buildDepError "zip-archive"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          ] ++ [
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/webdriver-0.9.0.1.tar.gz";
      sha256 = "135950889784b9d323c70ebf7ecd75b8df194489a303d85995b1fccc7549dff0";
      });
    }) // {
    package-description-override = "Name: webdriver\nVersion: 0.9.0.1\nCabal-Version: >= 1.10\nLicense: BSD3\nLicense-File: LICENSE\nAuthor: Adam Curtis\nMaintainer: kallisti.dev@gmail.com\nHomepage: https://github.com/kallisti-dev/hs-webdriver\nBug-Reports: https://github.com/kallisti-dev/hs-webdriver/issues\nCategory: Web, Browser, Testing, WebDriver, Selenium\nSynopsis: a Haskell client for the Selenium WebDriver protocol\nBuild-Type: Simple\nExtra-Source-Files: README.md, TODO.md, CHANGELOG.md, .ghci\nTested-With: GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.1\nDescription:\n        A Selenium WebDriver client for Haskell.\n        You can use it to automate browser sessions\n        for testing, system administration, etc.\n        .\n        For more information about Selenium itself, see\n        <http://seleniumhq.org/>\n        .\n        To find out what's been changed in this version and others,\n        see the change log at\n        <https://github.com/kallisti-dev/hs-webdriver/blob/master/CHANGELOG.md>\n\nSource-Repository head\n  type: git\n  location: git://github.com/kallisti-dev/hs-webdriver.git\n  \nFlag network-uri\n  description: Get Network.URI from the network-uri package\n  default: True\n\nFlag developer\n  description: Package development mode\n  default: False\n  manual: True\n\nLibrary\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options: -Wall\n  if flag(developer)\n    cpp-options: -DCABAL_BUILD_DEVELOPER\n  build-depends:   base == 4.*\n                 , aeson >= 0.6.2.0\n                 , http-client >= 0.3\n                 , http-types >= 0.8\n                 , text >= 0.11.3\n                 , bytestring >= 0.9\n                 , attoparsec >= 0.10\n                 , base64-bytestring >= 1.0\n                 , transformers >= 0.4\n                 , monad-control >= 0.3\n                 , transformers-base >= 0.1\n                 , lifted-base >= 0.1\n                 , zip-archive >= 0.1.1.8\n                 , directory > 1.0\n                 , filepath > 1.0\n                 , directory-tree >= 0.11\n                 , temporary >= 1.0\n                 , time > 1.0\n                 , unordered-containers >= 0.1.3\n                 , vector >= 0.3\n                 , exceptions >= 0.4\n                 , scientific >= 0.2\n                 , data-default-class\n                 , call-stack\n\n  if flag(network-uri)\n      build-depends: network-uri >= 2.6, network >= 2.6\n  else\n      build-depends: network-uri < 2.6, network >= 2.4 && < 2.6\n\n  exposed-modules: Test.WebDriver\n                   Test.WebDriver.Class\n                   Test.WebDriver.Monad\n                   Test.WebDriver.Session\n                   Test.WebDriver.Session.History\n                   Test.WebDriver.Config\n                   Test.WebDriver.Exceptions\n                   Test.WebDriver.Commands\n                   Test.WebDriver.Commands.Wait\n                   Test.WebDriver.Commands.Internal\n                   Test.WebDriver.Common.Profile\n                   Test.WebDriver.Common.Keys\n                   Test.WebDriver.Firefox.Profile\n                   Test.WebDriver.Chrome.Extension\n                   Test.WebDriver.Capabilities\n                   Test.WebDriver.Types\n                   Test.WebDriver.JSON\n                   Test.WebDriver.Utils\n                   Test.WebDriver.Internal\n                   Test.WebDriver.Exceptions.Internal\n";
    }