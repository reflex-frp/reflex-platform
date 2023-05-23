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
      identifier = { name = "ansi-terminal"; version = "0.11"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mike Pilgrem <public@pilgrem.com>, Roman Cheplyaka <roma@ro-che.info>";
      author = "Max Bolingbroke";
      homepage = "https://github.com/feuerbach/ansi-terminal";
      url = "";
      synopsis = "Simple ANSI terminal support, with Windows compatibility";
      description = "ANSI terminal support for Haskell: allows cursor movement,\nscreen clearing, color output, showing or hiding the\ncursor, and changing the title. Works on UNIX and Windows.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mintty" or (errorHandler.buildDepError "mintty"))
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          ];
        buildable = true;
        };
      exes = {
        "ansi-terminal-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
            ];
          buildable = if !flags.example then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ansi-terminal-0.11.tar.gz";
      sha256 = "c6611b9e51add41db3f79eac30066c06b33a6ca2a09e586b4b361d7f98303793";
      });
    }) // {
    package-description-override = "Name:                ansi-terminal\r\nVersion:             0.11\r\nCabal-Version:       >= 1.10\r\nCategory:            User Interfaces\r\nSynopsis:            Simple ANSI terminal support, with Windows compatibility\r\nDescription:         ANSI terminal support for Haskell: allows cursor movement,\r\n                     screen clearing, color output, showing or hiding the\r\n                     cursor, and changing the title. Works on UNIX and Windows.\r\nLicense:             BSD3\r\nLicense-File:        LICENSE\r\nAuthor:              Max Bolingbroke\r\nMaintainer:          Mike Pilgrem <public@pilgrem.com>, Roman Cheplyaka <roma@ro-che.info>\r\nHomepage:            https://github.com/feuerbach/ansi-terminal\r\nBuild-Type:          Simple\r\n\r\nExtra-Source-Files:     src/includes/Common-Include.hs\r\n                        src/includes/Common-Include-Emulator.hs\r\n                        src/includes/Common-Include-Enabled.hs\r\n                        src/includes/Common-Safe-Haskell.hs\r\n                        src/includes/Exports-Include.hs\r\n                        CHANGELOG.md\r\n                        README.md\r\n\r\nSource-repository head\r\n  type:     git\r\n  location: git://github.com/feuerbach/ansi-terminal.git\r\n\r\nFlag Example\r\n        Description:    Build the example application\r\n        Default:        False\r\n\r\nLibrary\r\n        Hs-Source-Dirs:         src\r\n        Exposed-Modules:        System.Console.ANSI\r\n                                System.Console.ANSI.Types\r\n                                System.Console.ANSI.Codes\r\n\r\n        Include-Dirs:           src/includes\r\n\r\n        Build-Depends:          base >= 4.3.0.0 && < 5\r\n                              , colour >=2.1.0\r\n        if os(windows)\r\n                Build-Depends:          containers >= 0.5.0.0\r\n                                      , mintty\r\n                                      , Win32 >= 2.0\r\n                Cpp-Options:            -DWINDOWS\r\n                Other-Modules:          System.Console.ANSI.Windows\r\n                                        System.Console.ANSI.Windows.Detect\r\n                                        System.Console.ANSI.Windows.Emulator\r\n                                        System.Console.ANSI.Windows.Emulator.Codes\r\n                                        System.Console.ANSI.Windows.Foreign\r\n                                        -- NB: used for fallback by the emulator\r\n                                        System.Console.ANSI.Unix\r\n                                        System.Win32.Compat\r\n        else\r\n                -- We assume any non-Windows platform is Unix\r\n                Cpp-Options:            -DUNIX\r\n                Other-Modules:          System.Console.ANSI.Unix\r\n\r\n        Default-Extensions:     CPP\r\n                                ForeignFunctionInterface\r\n\r\n        Ghc-Options:            -Wall\r\n        Default-Language:       Haskell2010\r\n\r\nExecutable ansi-terminal-example\r\n        Hs-Source-Dirs:         app\r\n        Main-Is:                Example.hs\r\n        Build-Depends:          base >= 4.3.0.0 && < 5\r\n                              , ansi-terminal\r\n                              , colour\r\n        Ghc-Options:            -Wall\r\n        if !flag(example)\r\n                Buildable:              False\r\n        Default-Language:       Haskell2010\r\n";
    }