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
    flags = { llvm = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "clock"; version = "0.8.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright © Cetin Sert 2009-2016, Eugene Kirpichov 2010, Finn Espen Gundersen 2013, Gerolf Seitz 2013, Mathieu Boespflug 2014 2015, Chris Done 2015, Dimitri Sabadie 2015, Christian Burger 2015, Mario Longobardi 2016, Alexander Vershilov 2021.";
      maintainer = "Cetin Sert <cetin@sert.works>, Corsis Research";
      author = "Cetin Sert <cetin@sert.works>, Corsis Research";
      homepage = "https://github.com/corsis/clock";
      url = "";
      synopsis = "High-resolution clock functions: monotonic, realtime, cputime.";
      description = "A package for convenient access to high-resolution clock and\ntimer functions of different operating systems via a unified API.\n\nPOSIX code and surface API was developed by Cetin Sert in 2009.\n\nWindows code was contributed by Eugene Kirpichov in 2010.\n\nFreeBSD code was contributed by Finn Espen Gundersen on 2013-10-14.\n\nOS X code was contributed by Gerolf Seitz on 2013-10-15.\n\nDerived @Generic@, @Typeable@ and other instances for @Clock@ and @TimeSpec@ was contributed by Mathieu Boespflug on 2014-09-17.\n\nCorrected dependency listing for @GHC < 7.6@ was contributed by Brian McKenna on 2014-09-30.\n\nWindows code corrected by Dimitri Sabadie on 2015-02-09.\n\nAdded @timeSpecAsNanoSecs@ as observed widely-used by Chris Done on 2015-01-06, exported correctly on 2015-04-20.\n\nImported Control.Applicative operators correctly for Haskell Platform on Windows on 2015-04-21.\n\nUnit tests and instance fixes by Christian Burger on 2015-06-25.\n\nRemoval of fromInteger : Integer -> TimeSpec by Cetin Sert on 2015-12-15.\n\nNew Linux-specific Clocks: MonotonicRaw, Boottime, MonotonicCoarse, RealtimeCoarse by Cetin Sert on 2015-12-15.\n\nReintroduction fromInteger : Integer -> TimeSpec by Cetin Sert on 2016-04-05.\n\nFixes for older Linux build failures introduced by new Linux-specific clocks by Mario Longobardi on 2016-04-18.\n\nRefreshment release in 2019-04 after numerous contributions.\n\nRefactoring for Windows, Mac implementation consistence by Alexander Vershilov on 2021-01-16.\n\n[Version Scheme]\nMajor-@/R/@-ewrite . New-@/F/@-unctionality . @/I/@-mprovementAndBugFixes . @/P/@-ackagingOnly\n\n* @PackagingOnly@ changes are made for quality assurance reasons.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "7.6") [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/clock-0.8.2.tar.gz";
      sha256 = "0b5db110c703e68b251d5883253a934b012110b45393fc65df1b095eb9a4e461";
      });
    }) // {
    package-description-override = "cabal-version: >= 1.10\nname:          clock\nversion:       0.8.2\nstability:     stable\nsynopsis:      High-resolution clock functions: monotonic, realtime, cputime.\ndescription:   A package for convenient access to high-resolution clock and\n               timer functions of different operating systems via a unified API.\n               .\n               POSIX code and surface API was developed by Cetin Sert in 2009.\n               .\n               Windows code was contributed by Eugene Kirpichov in 2010.\n               .\n               FreeBSD code was contributed by Finn Espen Gundersen on 2013-10-14.\n               .\n               OS X code was contributed by Gerolf Seitz on 2013-10-15.\n               .\n               Derived @Generic@, @Typeable@ and other instances for @Clock@ and @TimeSpec@ was contributed by Mathieu Boespflug on 2014-09-17.\n               .\n               Corrected dependency listing for @GHC < 7.6@ was contributed by Brian McKenna on 2014-09-30.\n               .\n               Windows code corrected by Dimitri Sabadie on 2015-02-09.\n               .\n               Added @timeSpecAsNanoSecs@ as observed widely-used by Chris Done on 2015-01-06, exported correctly on 2015-04-20.\n               .\n               Imported Control.Applicative operators correctly for Haskell Platform on Windows on 2015-04-21.\n               .\n               Unit tests and instance fixes by Christian Burger on 2015-06-25.\n               .\n               Removal of fromInteger : Integer -> TimeSpec by Cetin Sert on 2015-12-15.\n               .\n               New Linux-specific Clocks: MonotonicRaw, Boottime, MonotonicCoarse, RealtimeCoarse by Cetin Sert on 2015-12-15.\n               .\n               Reintroduction fromInteger : Integer -> TimeSpec by Cetin Sert on 2016-04-05.\n               .\n               Fixes for older Linux build failures introduced by new Linux-specific clocks by Mario Longobardi on 2016-04-18.\n               .\n               Refreshment release in 2019-04 after numerous contributions.\n               .\n               Refactoring for Windows, Mac implementation consistence by Alexander Vershilov on 2021-01-16.\n               .\n               [Version Scheme]\n               Major-@/R/@-ewrite . New-@/F/@-unctionality . @/I/@-mprovementAndBugFixes . @/P/@-ackagingOnly\n               .\n               * @PackagingOnly@ changes are made for quality assurance reasons.\n\ncopyright:     Copyright © Cetin Sert 2009-2016, Eugene Kirpichov 2010, Finn Espen Gundersen 2013, Gerolf Seitz 2013, Mathieu Boespflug 2014 2015, Chris Done 2015, Dimitri Sabadie 2015, Christian Burger 2015, Mario Longobardi 2016, Alexander Vershilov 2021.\nlicense:       BSD3\nlicense-file:  LICENSE\nauthor:        Cetin Sert <cetin@sert.works>, Corsis Research\nmaintainer:    Cetin Sert <cetin@sert.works>, Corsis Research\nhomepage:      https://github.com/corsis/clock\nbug-reports:   https://github.com/corsis/clock/issues\ncategory:      System\nbuild-type:    Simple\ntested-with: GHC==8.10.3, GHC==8.8.4, GHC==8.6.5\n\n\nsource-repository head\n    type:      git\n    location:  git://github.com/corsis/clock.git\n\n\nflag llvm\n    description: compile via LLVM\n    default    : False\n\n\nlibrary\n    default-language: Haskell2010\n    if impl (ghc < 7.6)\n      build-depends:       base >= 4.4 && <= 5, ghc-prim\n    build-depends:       base >= 2 && <= 5\n    exposed-modules:     System.Clock\n    default-extensions:          DeriveGeneric\n                         DeriveDataTypeable\n                         ForeignFunctionInterface\n                         ScopedTypeVariables\n                         ViewPatterns\n    if os(windows)\n      c-sources:         cbits/hs_clock_win32.c\n    include-dirs:        cbits\n    ghc-options:         -O3 -Wall\n\n    if flag(llvm)\n      ghc-options:       -fllvm -optlo-O3\n\n\ntest-suite test\n    default-language: Haskell2010\n    type:\n      exitcode-stdio-1.0\n    hs-source-dirs:\n      tests\n    main-is:\n      test.hs\n    build-depends:\n        base >= 4 && < 5\n      , tasty >= 0.10\n      , tasty-quickcheck\n      , clock\n\nbenchmark benchmarks\n    default-language: Haskell2010\n    type:\n      exitcode-stdio-1.0\n    hs-source-dirs:\n      bench\n    main-is:\n      benchmarks.hs\n    build-depends:\n        base >= 4 && < 5\n      , criterion\n      , clock\n";
    }