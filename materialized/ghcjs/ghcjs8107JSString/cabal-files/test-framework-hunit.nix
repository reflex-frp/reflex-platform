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
    flags = { base4 = true; base3 = false; };
    package = {
      specVersion = "1.6";
      identifier = { name = "test-framework-hunit"; version = "0.3.0.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Haskell Libraries <libraries@haskell.org>";
      author = "Max Bolingbroke <batterseapower@hotmail.com>";
      homepage = "https://batterseapower.github.io/test-framework/";
      url = "";
      synopsis = "HUnit support for the test-framework package.";
      description = "HUnit support for the test-framework package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."extensible-exceptions" or (errorHandler.buildDepError "extensible-exceptions"))
          ] ++ (if flags.base3
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else (pkgs.lib).optional (flags.base4) (hsPkgs."base" or (errorHandler.buildDepError "base")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/test-framework-hunit-0.3.0.2.tar.gz";
      sha256 = "95cb8ee02a850b164bfdabdf4dbc839d621361f3ac770ad21ea43a8bde360bf8";
      });
    }) // {
    package-description-override = "Name:                test-framework-hunit\r\nVersion:             0.3.0.2\r\nx-revision: 3\r\nCabal-Version:       >= 1.6\r\nCategory:            Testing\r\nSynopsis:            HUnit support for the test-framework package.\r\nLicense:             BSD3\r\nLicense-File:        LICENSE\r\nAuthor:              Max Bolingbroke <batterseapower@hotmail.com>\r\nMaintainer:          Haskell Libraries <libraries@haskell.org>\r\nHomepage:            https://batterseapower.github.io/test-framework/\r\nBug-Reports:         https://github.com/haskell/test-framework/issues\r\nBuild-Type:          Simple\r\nDescription:         HUnit support for the test-framework package.\r\n\r\nFlag Base4\r\n        Description:    Choose base version 4\r\n        Default:        True\r\n\r\nFlag Base3\r\n        Description:    Choose base version 3\r\n        Default:        False\r\n\r\n\r\nLibrary\r\n        Exposed-Modules:        Test.Framework.Providers.HUnit\r\n\r\n        Build-Depends:          test-framework >= 0.2.0, HUnit >= 1.2 && < 1.7, extensible-exceptions >= 0.1.1 && < 0.2.0\r\n        if flag(base3)\r\n                Build-Depends:          base >= 3 && < 4\r\n        else\r\n                if flag(base4)\r\n                        Build-Depends:          base >= 4 && < 5\r\n\r\n        Extensions:             TypeOperators\r\n                                MultiParamTypeClasses\r\n\r\n        Ghc-Options:            -Wall\r\n\r\nSource-Repository head\r\n  Type:     git\r\n  Location: https://github.com/haskell/test-framework.git\r\n  subdir:   hunit\r\n";
    }