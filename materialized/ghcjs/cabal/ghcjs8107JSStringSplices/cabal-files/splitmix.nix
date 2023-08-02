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
    flags = { optimised-mixer = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "splitmix"; version = "0.1.0.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Fast Splittable PRNG";
      description = "Pure Haskell implementation of SplitMix described in\n\nGuy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014.\nFast splittable pseudorandom number generators. In Proceedings\nof the 2014 ACM International Conference on Object Oriented\nProgramming Systems Languages & Applications (OOPSLA '14). ACM,\nNew York, NY, USA, 453-472. DOI:\n<https://doi.org/10.1145/2660193.2660195>\n\nThe paper describes a new algorithm /SplitMix/ for /splittable/\npseudorandom number generator that is quite fast: 9 64 bit arithmetic/logical\noperations per 64 bits generated.\n\n/SplitMix/ is tested with two standard statistical test suites (DieHarder and\nTestU01, this implementation only using the former) and it appears to be\nadequate for \"everyday\" use, such as Monte Carlo algorithms and randomized\ndata structures where speed is important.\n\nIn particular, it __should not be used for cryptographic or security applications__,\nbecause generated sequences of pseudorandom values are too predictable\n(the mixing functions are easily inverted, and two successive outputs\nsuffice to reconstruct the internal state).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) ((pkgs.lib).optional (!(compiler.isGhc && true)) (hsPkgs."time" or (errorHandler.buildDepError "time")));
        buildable = true;
        };
      tests = {
        "examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        "splitmix-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = true;
          };
        "montecarlo-pi" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        "montecarlo-pi-32" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        "splitmix-dieharder" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "splitmix-testu01" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          libs = [ (pkgs."testu01" or (errorHandler.sysDepError "testu01")) ];
          buildable = if !system.isLinux then false else true;
          };
        "initialization" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "comparison" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."tf-random" or (errorHandler.buildDepError "tf-random"))
            ];
          buildable = true;
          };
        "simple-sum" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        "range" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/splitmix-0.1.0.4.tar.gz";
      sha256 = "6d065402394e7a9117093dbb4530a21342c9b1e2ec509516c8a8d0ffed98ecaa";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               splitmix\nversion:            0.1.0.4\nx-revision:         1\nsynopsis:           Fast Splittable PRNG\ndescription:\n  Pure Haskell implementation of SplitMix described in\n  .\n  Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014.\n  Fast splittable pseudorandom number generators. In Proceedings\n  of the 2014 ACM International Conference on Object Oriented\n  Programming Systems Languages & Applications (OOPSLA '14). ACM,\n  New York, NY, USA, 453-472. DOI:\n  <https://doi.org/10.1145/2660193.2660195>\n  .\n  The paper describes a new algorithm /SplitMix/ for /splittable/\n  pseudorandom number generator that is quite fast: 9 64 bit arithmetic/logical\n  operations per 64 bits generated.\n  .\n  /SplitMix/ is tested with two standard statistical test suites (DieHarder and\n  TestU01, this implementation only using the former) and it appears to be\n  adequate for \"everyday\" use, such as Monte Carlo algorithms and randomized\n  data structures where speed is important.\n  .\n  In particular, it __should not be used for cryptographic or security applications__,\n  because generated sequences of pseudorandom values are too predictable\n  (the mixing functions are easily inverted, and two successive outputs\n  suffice to reconstruct the internal state).\n\nlicense:            BSD3\nlicense-file:       LICENSE\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nbug-reports:        https://github.com/haskellari/splitmix/issues\ncategory:           System, Random\nbuild-type:         Simple\ntested-with:\n    GHC ==7.0.4\n     || ==7.2.2\n     || ==7.4.2\n     || ==7.6.3\n     || ==7.8.4\n     || ==7.10.3\n     || ==8.0.2\n     || ==8.2.2\n     || ==8.4.4\n     || ==8.6.5\n     || ==8.8.4\n     || ==8.10.4\n     || ==9.0.2\n     || ==9.2.4\n     || ==9.4.1\n  , GHCJS ==8.4\n\nextra-source-files:\n  Changelog.md\n  make-hugs.sh\n  README.md\n  test-hugs.sh\n\nflag optimised-mixer\n  description: Use JavaScript for mix32\n  manual:      True\n  default:     False\n\nlibrary\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   src src-compat\n  exposed-modules:\n    System.Random.SplitMix\n    System.Random.SplitMix32\n\n  other-modules:\n    Data.Bits.Compat\n    System.Random.SplitMix.Init\n\n  -- dump-core\n  -- build-depends: dump-core\n  -- ghc-options: -fplugin=DumpCore -fplugin-opt DumpCore:core-html\n\n  build-depends:\n      base     >=4.3     && <4.18\n    , deepseq  >=1.3.0.0 && <1.5\n\n  if flag(optimised-mixer)\n    cpp-options: -DOPTIMISED_MIX32=1\n\n  -- We don't want to depend on time, nor unix or Win32 packages\n  -- because it's valuable that splitmix and QuickCheck doesn't\n  -- depend on about anything\n\n  if impl(ghcjs)\n    cpp-options: -DSPLITMIX_INIT_GHCJS=1\n\n  else\n    if impl(ghc)\n      cpp-options: -DSPLITMIX_INIT_C=1\n\n      if os(windows)\n        c-sources: cbits-win/init.c\n\n      else\n        c-sources: cbits-unix/init.c\n\n    else\n      cpp-options:   -DSPLITMIX_INIT_COMPAT=1\n      build-depends: time >=1.2.0.3 && <1.13\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/splitmix.git\n\nbenchmark comparison\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   bench\n  main-is:          Bench.hs\n  build-depends:\n      base\n    , containers  >=0.4.2.1 && <0.7\n    , criterion   >=1.1.0.0 && <1.6\n    , random\n    , splitmix\n    , tf-random   >=0.5     && <0.6\n\nbenchmark simple-sum\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   bench\n  main-is:          SimpleSum.hs\n  build-depends:\n      base\n    , random\n    , splitmix\n\nbenchmark range\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   bench src-compat\n  main-is:          Range.hs\n  other-modules:    Data.Bits.Compat\n  build-depends:\n      base\n    , clock     >=0.8 && <0.9\n    , random\n    , splitmix\n\ntest-suite examples\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   tests\n  main-is:          Examples.hs\n  build-depends:\n      base\n    , HUnit     ==1.3.1.2 || >=1.6.0.0 && <1.7\n    , splitmix\n\ntest-suite splitmix-tests\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   tests\n  main-is:          Tests.hs\n  other-modules:\n    MiniQC\n    Uniformity\n\n  build-depends:\n      base\n    , base-compat           >=0.11.1  && <0.13\n    , containers            >=0.4.0.0 && <0.7\n    , HUnit                 ==1.3.1.2 || >=1.6.0.0 && <1.7\n    , math-functions        ==0.1.7.0 || >=0.3.3.0 && <0.4\n    , splitmix\n    , test-framework        >=0.8.2.0 && <0.9\n    , test-framework-hunit  >=0.3.0.2 && <0.4\n\ntest-suite montecarlo-pi\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   tests\n  main-is:          SplitMixPi.hs\n  build-depends:\n      base\n    , splitmix\n\ntest-suite montecarlo-pi-32\n  type:             exitcode-stdio-1.0\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   tests\n  main-is:          SplitMixPi32.hs\n  build-depends:\n      base\n    , splitmix\n\ntest-suite splitmix-dieharder\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  ghc-options:      -Wall -threaded -rtsopts\n  hs-source-dirs:   tests\n  main-is:          Dieharder.hs\n  build-depends:\n      async                  >=2.2.1    && <2.3\n    , base\n    , base-compat-batteries  >=0.10.5   && <0.13\n    , bytestring             >=0.9.1.8  && <0.12\n    , deepseq\n    , process                >=1.0.1.5  && <1.7\n    , random\n    , splitmix\n    , tf-random              >=0.5      && <0.6\n    , vector                 >=0.11.0.0 && <0.13\n\ntest-suite splitmix-testu01\n  if !os(linux)\n    buildable: False\n\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  ghc-options:      -Wall -threaded -rtsopts\n  hs-source-dirs:   tests\n  main-is:          TestU01.hs\n  c-sources:        tests/cbits/testu01.c\n  extra-libraries:  testu01\n  build-depends:\n      base\n    , base-compat-batteries  >=0.10.5   && <0.13\n    , splitmix\n\ntest-suite initialization\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  ghc-options:      -Wall -threaded -rtsopts\n  hs-source-dirs:   tests\n  main-is:          Initialization.hs\n  build-depends:\n      base\n    , HUnit     ==1.3.1.2 || >=1.6.0.0 && <1.7\n    , splitmix\n";
    }