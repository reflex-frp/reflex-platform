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
    flags = { lifted = false; build-examples = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "shelly"; version = "1.9.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Greg Weber <greg@gregweber.info>";
      author = "Greg Weber, Petr Rockai";
      homepage = "https://github.com/yesodweb/Shelly.hs";
      url = "";
      synopsis = "shell-like (systems) programming in Haskell";
      description = "Shelly provides convenient systems programming in Haskell,\nsimilar in spirit to POSIX shells. Shelly:\n\n* is aimed at convenience and getting things done rather than\nbeing a demonstration of elegance.\n\n* has detailed and useful error messages\n\n* maintains its own environment, making it thread-safe.\n\n* is modern, using Text filepath/directory\n\nShelly is originally forked from the Shellish package.\n\nSee the shelly-extra package for additional functionality.\n\nAn overview is available in the README: <https://github.com/yesodweb/Shelly.hs/blob/master/README.md>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."enclosed-exceptions" or (errorHandler.buildDepError "enclosed-exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      exes = {
        "drain" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        "run-handles" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        "Color" = {
          depends = (pkgs.lib).optionals (flags.build-examples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."shelly" or (errorHandler.buildDepError "shelly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.build-examples then true else false;
          };
        };
      tests = {
        "shelly-testsuite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-contrib" or (errorHandler.buildDepError "hspec-contrib"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."enclosed-exceptions" or (errorHandler.buildDepError "enclosed-exceptions"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/shelly-1.9.0.tar.gz";
      sha256 = "5eb5fd4fc105e218cef6cfa10971d299ad660324e6a6006b8cccc31edf39aace";
      });
    }) // {
    package-description-override = "Name:       shelly\r\n\r\nVersion:     1.9.0\r\nx-revision: 1\r\nSynopsis:    shell-like (systems) programming in Haskell\r\n\r\nDescription: Shelly provides convenient systems programming in Haskell,\r\n             similar in spirit to POSIX shells. Shelly:\r\n             .\r\n               * is aimed at convenience and getting things done rather than\r\n                 being a demonstration of elegance.\r\n             .\r\n               * has detailed and useful error messages\r\n             .\r\n               * maintains its own environment, making it thread-safe.\r\n             .\r\n               * is modern, using Text filepath/directory\r\n             .\r\n             Shelly is originally forked from the Shellish package.\r\n             .\r\n             See the shelly-extra package for additional functionality.\r\n             .\r\n             An overview is available in the README: <https://github.com/yesodweb/Shelly.hs/blob/master/README.md>\r\n\r\n\r\nHomepage:            https://github.com/yesodweb/Shelly.hs\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nAuthor:              Greg Weber, Petr Rockai\r\nMaintainer:          Greg Weber <greg@gregweber.info>\r\nCategory:            Development\r\nBuild-type:          Simple\r\nCabal-version:       >=1.8\r\n\r\n-- for the sdist of the test suite\r\nextra-source-files: test/src/*.hs\r\n                    test/examples/*.sh\r\n                    test/examples/*.hs\r\n                    test/data/zshrc\r\n                    test/data/nonascii.txt\r\n                    test/data/symlinked_dir/hoge_file\r\n                    test/testall\r\n                    README.md\r\n                    ChangeLog.md\r\n\r\nLibrary\r\n  Exposed-modules: Shelly, Shelly.Lifted, Shelly.Pipe, Shelly.Unix\r\n  other-modules:   Shelly.Base, Shelly.Find, Shelly.Directory\r\n  hs-source-dirs: src\r\n\r\n  Build-depends:\r\n    containers                >= 0.4.2.0,\r\n    time                      >= 1.3 && < 1.10,\r\n    directory                 >= 1.3.0.0 && < 1.4.0.0,\r\n    mtl                       >= 2,\r\n    process                   >= 1.0,\r\n    unix-compat               < 0.6,\r\n    unix,\r\n    filepath,\r\n    monad-control             >= 0.3.2 && < 1.1,\r\n    lifted-base,\r\n    lifted-async,\r\n    exceptions                >= 0.6,\r\n    enclosed-exceptions,\r\n    text, bytestring, async, transformers, transformers-base\r\n\r\n  build-depends: base >= 4.9\r\n  if impl(ghc >= 7.6.1)\r\n    build-depends:\r\n        base >= 4.6 && < 5\r\n  else\r\n    build-depends:\r\n      base >= 4 && < 5\r\n\r\n  ghc-options: -Wall\r\n\r\n  if impl(ghc >= 7.6.1)\r\n      CPP-Options: -DNO_PRELUDE_CATCH\r\n\r\n  extensions:\r\n    CPP\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/yesodweb/Shelly.hs\r\n\r\nFlag lifted\r\n   Description: run the tests against Shelly.Lifted\r\n   Default: False\r\n\r\nTest-Suite shelly-testsuite\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: src test/src\r\n  main-is: TestMain.hs\r\n  other-modules:\r\n    CopySpec\r\n    EnvSpec\r\n    FailureSpec\r\n    FindSpec\r\n    Help\r\n    LiftedSpec\r\n    MoveSpec\r\n    ReadFileSpec\r\n    RmSpec\r\n    RunSpec\r\n    SshSpec\r\n    Shelly\r\n    Shelly.Base\r\n    Shelly.Find\r\n    Shelly.Lifted\r\n    TestInit\r\n    WhichSpec\r\n    WriteSpec\r\n\r\n  ghc-options: -Wall -fwarn-tabs -funbox-strict-fields -threaded\r\n               -fno-warn-unused-do-bind -fno-warn-type-defaults\r\n\r\n\r\n  extensions: OverloadedStrings, ExtendedDefaultRules\r\n\r\n  if flag(lifted)\r\n     cpp-options: -DLIFTED\r\n\r\n  build-depends:\r\n    base                      >= 4.6,\r\n    text                      >= 0.11,\r\n    async,\r\n    bytestring                >= 0.10,\r\n    containers                >= 0.5.0.0,\r\n    directory                 >= 1.3.0.0 && < 1.4.0.0,\r\n    process                   >= 1.1.0,\r\n    unix-compat               < 0.6,\r\n    unix,\r\n    time                      >= 1.3 && < 1.10,\r\n    mtl                       >= 2,\r\n    HUnit                     >= 1.2,\r\n    hspec                     >= 2.0,\r\n    hspec-contrib,\r\n    transformers,\r\n    transformers-base,\r\n    filepath,\r\n    monad-control,\r\n    lifted-base,\r\n    lifted-async,\r\n    enclosed-exceptions,\r\n    exceptions\r\n\r\n  if impl(ghc < 8.0)\r\n    build-depends:       fail >= 4.9 && < 4.10\r\n\r\n  extensions:\r\n    CPP\r\n\r\nFlag build-examples\r\n   Description: build some example programs\r\n   Default: False\r\n   Manual: True\r\n\r\n-- demonstarated that command output in Shellish was not shown until after the command finished\r\n-- not necessary anymore\r\nExecutable drain\r\n  hs-source-dirs: test/examples\r\n  main-is: drain.hs\r\n  if flag(build-examples)\r\n    buildable: True\r\n\r\n    build-depends: base                      >= 4.6\r\n                 , shelly\r\n                 , text\r\n\r\n    extensions:\r\n      CPP\r\n  else\r\n    buildable: False\r\n\r\nExecutable run-handles\r\n  hs-source-dirs: test/examples\r\n  main-is: run-handles.hs\r\n  if flag(build-examples)\r\n    buildable: True\r\n\r\n    build-depends: base                      >= 4.6\r\n                 , shelly\r\n                 , text\r\n\r\n    extensions:\r\n      CPP\r\n  else\r\n    buildable: False\r\n\r\nExecutable Color\r\n  hs-source-dirs: test/examples\r\n  main-is: color.hs\r\n  if flag(build-examples)\r\n    buildable: True\r\n\r\n    build-depends: base                      >= 4.6\r\n                 , process\r\n                 , shelly\r\n                 , text\r\n  else\r\n    buildable: False\r\n";
    }