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
    flags = { old-locale = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "time-compat"; version = "1.9.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Ashley Yakeley";
      homepage = "https://github.com/haskellari/time-compat";
      url = "";
      synopsis = "Compatibility package for time";
      description = "This packages tries to compat as much of @time@ features as possible.\n\n/TODO:/\n\n* Difference type @ParseTime@ and @FormatTime@ instances are missing.\n\n* Formatting varies depending on underlying @time@ version\n\n* @dayFractionToTimeOfDay@ on extreme values";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (if flags.old-locale
          then [
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ]
          else [
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ])) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ];
        buildable = true;
        };
      tests = {
        "instances" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            ];
          buildable = true;
          };
        "main" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = if !(compiler.isGhc && (compiler.version).ge "7.4")
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/time-compat-1.9.5.tar.gz";
      sha256 = "3126b267d19f31d52a3c36f13a8788be03242f829a5bddd8a3084e134d01e3a6";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               time-compat\nversion:            1.9.5\nx-revision:         1\nsynopsis:           Compatibility package for time\ndescription:\n  This packages tries to compat as much of @time@ features as possible.\n  .\n  /TODO:/\n  .\n  * Difference type @ParseTime@ and @FormatTime@ instances are missing.\n  .\n  * Formatting varies depending on underlying @time@ version\n  .\n  * @dayFractionToTimeOfDay@ on extreme values\n\ncategory:           Time, Compatibility\nlicense:            BSD3\nlicense-file:       LICENSE\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nauthor:             Ashley Yakeley\nhomepage:           https://github.com/haskellari/time-compat\nbug-reports:        https://github.com/haskellari/time-compat/issues\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\ntested-with:\n  GHC ==7.0.4\n   || ==7.2.2\n   || ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.3\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/time-compat.git\n\nflag old-locale\n  description: If true, use old-locale, otherwise use time 1.5 or newer.\n  manual:      False\n  default:     False\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  other-extensions: CPP\n\n  if impl(ghc >=7.2)\n    default-extensions: Trustworthy\n\n  build-depends:\n      base          >=4.3     && <4.16\n    , base-orphans  >=0.8.1   && <0.9\n    , deepseq       >=1.3.0.0 && <1.4 || >=1.4.1.1 && <1.5\n    , time          >=1.2     && <1.3 || >=1.4 && <1.7 || >=1.8 && <1.9 || >=1.9.2 && <1.9.4 || >=1.10 && <1.10.1 || >=1.11 && <1.11.2\n\n  if flag(old-locale)\n    build-depends:\n        old-locale  >=1.0.0.2 && <1.1\n      , time        >=0       && <1.5\n\n  else\n    build-depends: time >=1.5\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        fail        >=4.9.0.0 && <4.10\n      , semigroups  >=0.18.5  && <0.20\n\n  exposed-modules:\n    Data.Time.Calendar.Compat\n    Data.Time.Calendar.Easter.Compat\n    Data.Time.Calendar.Julian.Compat\n    Data.Time.Calendar.Month.Compat\n    Data.Time.Calendar.MonthDay.Compat\n    Data.Time.Calendar.OrdinalDate.Compat\n    Data.Time.Calendar.Quarter.Compat\n    Data.Time.Calendar.WeekDate.Compat\n    Data.Time.Clock.Compat\n    Data.Time.Clock.POSIX.Compat\n    Data.Time.Clock.System.Compat\n    Data.Time.Clock.TAI.Compat\n    Data.Time.Compat\n    Data.Time.Format.Compat\n    Data.Time.Format.ISO8601.Compat\n    Data.Time.LocalTime.Compat\n\n  other-modules:\n    Data.Format\n    Data.Time.Calendar.Private\n    Data.Time.Calendar.Types\n    Data.Time.Orphans\n\ntest-suite instances\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test-instances\n  main-is:          Test.hs\n  build-depends:\n      base\n    , deepseq\n    , HUnit        >=1.3.1 && <1.3.2 || >=1.6.0.0 && <1.7\n    , time-compat\n\n-- This test-suite is from time library\n-- Changes:\n-- * imports: Data.Time -> Data.Time.Compat etc\n-- * disabled Test.Format.ParseTime\n-- * Test.Format.Format has also trees disabled\n-- * Test.Format.Compile doesn't work\n-- * disabled 'TimeOfDay minBound 0 0' (Test.LocalTime.Time)\n--\ntest-suite main\n  if !impl(ghc >=7.4)\n    buildable: False\n\n  default-language:   Haskell2010\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     test/main\n  default-extensions:\n    CPP\n    DeriveDataTypeable\n    ExistentialQuantification\n    FlexibleInstances\n    MultiParamTypeClasses\n    Rank2Types\n    ScopedTypeVariables\n    StandaloneDeriving\n    TupleSections\n    UndecidableInstances\n\n  ghc-options:        -Wall -fwarn-tabs\n  build-depends:\n      base\n    , base-compat       >=0.10.5 && <0.12\n    , deepseq\n    , QuickCheck        >=2.13   && <2.15\n    , tagged            >=0.8.6  && <0.9\n    , tasty             >=1.2.1  && <1.5\n    , tasty-hunit       >=0.10   && <0.11\n    , tasty-quickcheck  >=0.10   && <0.11\n    , time-compat\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        fail        >=4.9.0.0 && <4.10\n      , semigroups  >=0.18.5  && <0.20\n\n  build-depends:      time\n  main-is:            Main.hs\n  other-modules:\n    Test.Arbitrary\n    Test.Calendar.AddDays\n    Test.Calendar.AddDaysRef\n    Test.Calendar.CalendarProps\n    Test.Calendar.Calendars\n    Test.Calendar.CalendarsRef\n    Test.Calendar.ClipDates\n    Test.Calendar.ClipDatesRef\n    Test.Calendar.ConvertBack\n    Test.Calendar.Duration\n    Test.Calendar.Easter\n    Test.Calendar.EasterRef\n    Test.Calendar.LongWeekYears\n    Test.Calendar.LongWeekYearsRef\n    Test.Calendar.MonthDay\n    Test.Calendar.MonthDayRef\n    Test.Calendar.Valid\n    Test.Calendar.Week\n    Test.Clock.Conversion\n    Test.Clock.Resolution\n    Test.Clock.TAI\n    Test.Format.Compile\n    Test.Format.Format\n    Test.Format.ISO8601\n    Test.Format.ParseTime\n    Test.LocalTime.CalendarDiffTime\n    Test.LocalTime.Time\n    Test.LocalTime.TimeOfDay\n    Test.LocalTime.TimeRef\n    Test.TestUtil\n    Test.Types\n";
    }