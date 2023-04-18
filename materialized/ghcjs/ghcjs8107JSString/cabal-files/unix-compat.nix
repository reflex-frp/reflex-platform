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
    flags = { old-time = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "unix-compat"; version = "0.5.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Björn Bringert, Duncan Coutts, Jacob Stanley, Bryan O'Sullivan";
      homepage = "http://github.com/jacobstanley/unix-compat";
      url = "";
      synopsis = "Portable POSIX-compatibility layer.";
      description = "This package provides portable implementations of parts\nof the unix package. This package re-exports the unix\npackage when available. When it isn't available,\nportable implementations are used.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ] ++ (if flags.old-time
            then [
              (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
              ] ++ [
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              ]
            else [
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              ])
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."msvcrt" or (errorHandler.sysDepError "msvcrt"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unix-compat-0.5.3.tar.gz";
      sha256 = "0893b597ea0db406429d0d563506af6755728eface0e1981f9392122db88e5c8";
      });
    }) // {
    package-description-override = "name:           unix-compat\nversion:        0.5.3\nsynopsis:       Portable POSIX-compatibility layer.\ndescription:    This package provides portable implementations of parts\n                of the unix package. This package re-exports the unix\n                package when available. When it isn't available,\n                portable implementations are used.\n\nhomepage:       http://github.com/jacobstanley/unix-compat\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Björn Bringert, Duncan Coutts, Jacob Stanley, Bryan O'Sullivan\nmaintainer:     Jacob Stanley <jacob@stanley.io>\ncategory:       System\nbuild-type:     Simple\ncabal-version:  >= 1.10\n\nsource-repository head\n  type:     git\n  location: git://github.com/jacobstanley/unix-compat.git\n\nflag old-time\n  description: build against old-time package\n  default: False\n\nLibrary\n  default-language: Haskell2010\n  hs-source-dirs: src\n  ghc-options: -Wall\n  build-depends: base == 4.*\n\n  exposed-modules:\n    System.PosixCompat\n    System.PosixCompat.Extensions\n    System.PosixCompat.Files\n    System.PosixCompat.Temp\n    System.PosixCompat.Time\n    System.PosixCompat.Types\n    System.PosixCompat.Unistd\n    System.PosixCompat.User\n\n  if os(windows)\n    c-sources:\n      cbits/HsUname.c\n      cbits/mktemp.c\n\n    extra-libraries: msvcrt\n    build-depends: Win32 >= 2.5.0.0\n\n    if flag(old-time)\n      build-depends: old-time >= 1.0.0.0 && < 1.2.0.0\n      cpp-options: -DOLD_TIME\n\n      if impl(ghc < 7)\n        build-depends: directory == 1.0.*\n        cpp-options: -DDIRECTORY_1_0\n      else\n        build-depends: directory == 1.1.*\n    else\n      build-depends: time >= 1.0 && < 1.10\n      build-depends: directory >= 1.2 && < 1.4\n\n    other-modules:\n      System.PosixCompat.Internal.Time\n\n  else\n    build-depends: unix >= 2.4 && < 2.9\n    include-dirs: include\n    includes: HsUnixCompat.h\n    install-includes: HsUnixCompat.h\n    c-sources: cbits/HsUnixCompat.c\n    if os(solaris)\n      cc-options: -DSOLARIS\n";
    }