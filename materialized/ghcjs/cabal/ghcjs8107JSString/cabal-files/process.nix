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
      specVersion = "1.10";
      identifier = { name = "process"; version = "1.6.16.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Process libraries";
      description = "This package contains libraries for dealing with system processes.\n\nThe typed-process package is a more recent take on a process API,\nwhich uses this package internally. It features better binary\nsupport, easier concurrency, and a more composable API. You can\nread more about it at\n<https://github.com/fpco/typed-process/#readme>.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        libs = (pkgs.lib).optionals (system.isWindows) [
          (pkgs."kernel32" or (errorHandler.sysDepError "kernel32"))
          (pkgs."ole32" or (errorHandler.sysDepError "ole32"))
          (pkgs."rpcrt4" or (errorHandler.sysDepError "rpcrt4"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/process-1.6.16.0.tar.gz";
      sha256 = "bbc21380d1271aa0dd57934f0b7a4f790f3f6cd12a27eed3eefcd46677ca9328";
      });
    }) // {
    package-description-override = "name:          process\nversion:       1.6.16.0\n-- NOTE: Don't forget to update ./changelog.md\nlicense:       BSD3\nlicense-file:  LICENSE\nmaintainer:    libraries@haskell.org\nbug-reports:   https://github.com/haskell/process/issues\nsynopsis:      Process libraries\ncategory:      System\nbuild-type:    Configure\ncabal-version: >=1.10\ndescription:\n    This package contains libraries for dealing with system processes.\n    .\n    The typed-process package is a more recent take on a process API,\n    which uses this package internally. It features better binary\n    support, easier concurrency, and a more composable API. You can\n    read more about it at\n    <https://github.com/fpco/typed-process/#readme>.\n\nextra-source-files:\n    aclocal.m4\n    changelog.md\n    configure\n    configure.ac\n    include/HsProcessConfig.h.in\n    process.buildinfo\n    exes/echo.bat\n    exes/subdir/echo.bat\n    cbits/posix/common.h\n\nextra-tmp-files:\n    autom4te.cache\n    config.log\n    config.status\n    include/HsProcessConfig.h\n\nsource-repository head\n    type:     git\n    location: https://github.com/haskell/process.git\n\nlibrary\n    default-language: Haskell2010\n    other-extensions:\n        BangPatterns\n        CPP\n        InterruptibleFFI\n        RecordWildCards\n        Trustworthy\n        Safe\n\n    exposed-modules:\n        System.Cmd\n        System.Process\n        System.Process.Internals\n    other-modules: System.Process.Common\n    if os(windows)\n        c-sources:\n            cbits/win32/runProcess.c\n        other-modules: System.Process.Windows\n        build-depends: Win32 >=2.4 && < 2.14\n        -- ole32 and rpcrt4 are needed to create GUIDs for unique named pipes\n        -- for process.\n        extra-libraries: kernel32, ole32, rpcrt4\n        cpp-options: -DWINDOWS\n    else\n        c-sources:\n            cbits/posix/runProcess.c\n            cbits/posix/fork_exec.c\n            cbits/posix/posix_spawn.c\n            cbits/posix/find_executable.c\n        other-modules: System.Process.Posix\n        build-depends: unix >= 2.5 && < 2.9\n\n    include-dirs: include\n    includes:\n        runProcess.h\n    install-includes:\n        runProcess.h\n        processFlags.h\n\n    ghc-options: -Wall\n\n    build-depends: base      >= 4.10 && < 4.18,\n                   directory >= 1.1 && < 1.4,\n                   filepath  >= 1.2 && < 1.5,\n                   deepseq   >= 1.1 && < 1.5\n\ntest-suite test\n  default-language: Haskell2010\n  hs-source-dirs: test\n  main-is: main.hs\n  type: exitcode-stdio-1.0\n  -- Add otherwise redundant bounds on base since GHC's build system runs\n  -- `cabal check`, which mandates bounds on base.\n  build-depends: base >= 4 && < 5\n               , bytestring\n               , directory\n               , process\n  ghc-options: -threaded\n               -with-rtsopts \"-N\"\n  if os(windows)\n        cpp-options: -DWINDOWS\n";
    }