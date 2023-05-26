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
    flags = { ofd-locking = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "lukko"; version = "0.1.1.3"; };
      license = "GPL-2.0-or-later AND BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "File locking";
      description = "This package provides access to platform dependent file locking APIs:\n\n* <https://www.gnu.org/software/libc/manual/html_node/Open-File-Description-Locks.html Open file descriptor locking> on Linux (\"Lukko.OFD\")\n* BSD-style @flock(2)@ locks on UNIX platforms (\"Lukko.FLock\")\n* Windows locking via <https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-lockfilee LockFileEx> (\"Lukko.Windows\")\n* No-op locking, which throws exceptions (\"Lukko.NoOp\")\n* \"Lukko\" module exports the best option for the target platform with uniform API.\n\nThere are alternative file locking packages:\n\n* \"GHC.IO.Handle.Lock\" in @base >= 4.10@ is good enough for most use cases.\nHowever, uses only 'Handle's so these locks cannot be used for intra-process locking.\n(You should use e.g. 'MVar' in addition).\n\n* <https://hackage.haskell.org/package/filelock filelock> doesn't support OFD locking.\n\n/Lukko/ means lock in Finnish.\n\nSubmodules \"Lukko.OFD\", \"Lukko.Windows\" etc are available based on following conditions.\n\n@\nif os(windows)\n\\  cpp-options: -DHAS_WINDOWS_LOCK\n\nelif (os(linux) && flag(ofd-locking))\n\\  cpp-options: -DHAS_OFD_LOCKING\n\\  cpp-options: -DHAS_FLOCK\n\nelif !(os(solaris) || os(aix))\n\\  cpp-options: -DHAS_FLOCK\n@\n\n\"Lukko.FLock\" is available on not (Windows or Solaris or AIX).\n\"Lukko.NoOp\" is always available.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      tests = {
        "test-thread" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."lukko" or (errorHandler.buildDepError "lukko"))
            (hsPkgs."singleton-bool" or (errorHandler.buildDepError "singleton-bool"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
          buildable = true;
          };
        "test-process" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."lukko" or (errorHandler.buildDepError "lukko"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lukko-0.1.1.3.tar.gz";
      sha256 = "a80efb60cfa3dae18682c01980d76d5f7e413e191cd186992e1bf7388d48ab1f";
      });
    }) // {
    package-description-override = "cabal-version:      2.2\nname:               lukko\nversion:            0.1.1.3\nx-revision:         3\nsynopsis:           File locking\ncategory:           System, Concurrency\ndescription:\n  This package provides access to platform dependent file locking APIs:\n  .\n  * <https://www.gnu.org/software/libc/manual/html_node/Open-File-Description-Locks.html Open file descriptor locking> on Linux (\"Lukko.OFD\")\n  * BSD-style @flock(2)@ locks on UNIX platforms (\"Lukko.FLock\")\n  * Windows locking via <https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-lockfilee LockFileEx> (\"Lukko.Windows\")\n  * No-op locking, which throws exceptions (\"Lukko.NoOp\")\n  * \"Lukko\" module exports the best option for the target platform with uniform API.\n  .\n  There are alternative file locking packages:\n  .\n  * \"GHC.IO.Handle.Lock\" in @base >= 4.10@ is good enough for most use cases.\n  However, uses only 'Handle's so these locks cannot be used for intra-process locking.\n  (You should use e.g. 'MVar' in addition).\n  .\n  * <https://hackage.haskell.org/package/filelock filelock> doesn't support OFD locking.\n  .\n  /Lukko/ means lock in Finnish.\n  .\n  Submodules \"Lukko.OFD\", \"Lukko.Windows\" etc are available based on following conditions.\n  .\n  @\n  if os(windows)\n  \\  cpp-options: -DHAS_WINDOWS_LOCK\n  .\n  elif (os(linux) && flag(ofd-locking))\n  \\  cpp-options: -DHAS_OFD_LOCKING\n  \\  cpp-options: -DHAS_FLOCK\n  .\n  elif !(os(solaris) || os(aix))\n  \\  cpp-options: -DHAS_FLOCK\n  @\n  .\n  \"Lukko.FLock\" is available on not (Windows or Solaris or AIX).\n  \"Lukko.NoOp\" is always available.\n\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            GPL-2.0-or-later AND BSD-3-Clause\nlicense-files:\n  LICENSE\n  LICENSE.GPLv2\n  LICENSE.GPLv3\n\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\ntested-with:\n  GHC ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/lukko/\n\nflag ofd-locking\n  default:     True\n  manual:      True\n  description:\n    Enable open file descriptor locking. Available on Linux (kernel 3.15, released Jun 8, 2014).\n\nlibrary\n  default-language:   Haskell2010\n  hs-source-dirs:     src\n  build-depends:      base >=4.5 && <4.18\n  build-tool-depends: hsc2hs:hsc2hs >=0.67 && <0.69\n\n  -- Main library module\n  exposed-modules:\n    Lukko\n    Lukko.NoOp\n\n  if os(windows)\n    hs-source-dirs:  src-windows\n    cpp-options:     -DUSE_WINDOWS_LOCK\n    exposed-modules: Lukko.Windows\n    c-sources:       cbits/windows.c\n\n  elif (os(linux) && flag(ofd-locking))\n    hs-source-dirs:  src-ofd\n    hs-source-dirs:  src-flock\n    hs-source-dirs:  src-unix\n    cpp-options:     -DUSE_OFD_LOCKING\n    exposed-modules: Lukko.OFD\n\n  elif !(os(solaris) || os(aix))\n    hs-source-dirs: src-flock\n    hs-source-dirs: src-unix\n    cpp-options:    -DUSE_FLOCK\n\n  else\n    hs-source-dirs: src-unix\n    cpp-options:    -DUSE_NOOP\n\n  -- Cabal check is silly\n  if (!os(windows) && !(os(solaris) || os(aix)))\n    exposed-modules: Lukko.FLock\n\n  other-modules:\n    Lukko.Internal.FD\n    Lukko.Internal.FillBytes\n    Lukko.Internal.HandleToFD\n    Lukko.Internal.Types\n\ntest-suite test-thread\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test\n  main-is:          Tests.hs\n  ghc-options:      -threaded\n  build-depends:\n    , async                   ^>=2.2.2\n    , base\n    , filepath                ^>=1.3.0.0 || ^>=1.4.0.0\n    , lukko\n    , singleton-bool          ^>=0.1.5\n    , tasty                   ^>=1.4.0.1\n    , tasty-expected-failure  ^>=0.11.1.2 || ^>=0.12.2\n    , tasty-hunit             ^>=0.10.0.2\n    , temporary               ^>=1.3\n\n  if !impl(ghc >=7.8)\n    build-depends: tagged ^>=0.8.5\n\n  if os(windows)\n    cpp-options: -DHAS_WINDOWS_LOCK\n\n  elif (os(linux) && flag(ofd-locking))\n    cpp-options: -DHAS_OFD_LOCKING\n    cpp-options: -DHAS_FLOCK\n\n  elif !(os(solaris) || os(aix))\n    cpp-options: -DHAS_FLOCK\n\ntest-suite test-process\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test\n  main-is:          TestProcess.hs\n  ghc-options:      -threaded\n  build-depends:\n    , base\n    , bytestring  >=0.9.2.1 && <0.12\n    , lukko\n\n  if os(windows)\n    cpp-options: -DHAS_WINDOWS_LOCK\n\n  elif (os(linux) && flag(ofd-locking))\n    cpp-options: -DHAS_OFD_LOCKING\n    cpp-options: -DHAS_FLOCK\n\n  elif !(os(solaris) || os(aix))\n    cpp-options: -DHAS_FLOCK\n";
    }