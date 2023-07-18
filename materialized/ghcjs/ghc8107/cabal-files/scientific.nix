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
    flags = { bytestring-builder = false; integer-simple = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "scientific"; version = "0.3.6.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Bas van Dijk <v.dijk.bas@gmail.com>";
      author = "Bas van Dijk";
      homepage = "https://github.com/basvandijk/scientific";
      url = "";
      synopsis = "Numbers represented using scientific notation";
      description = "\"Data.Scientific\" provides the number type 'Scientific'. Scientific numbers are\narbitrary precision and space efficient. They are represented using\n<http://en.wikipedia.org/wiki/Scientific_notation scientific notation>.\nThe implementation uses a coefficient @c :: 'Integer'@ and a base-10 exponent\n@e :: 'Int'@. A scientific number corresponds to the\n'Fractional' number: @'fromInteger' c * 10 '^^' e@.\n\nNote that since we're using an 'Int' to represent the exponent these numbers\naren't truly arbitrary precision. I intend to change the type of the exponent\nto 'Integer' in a future release.\n\nThe main application of 'Scientific' is to be used as the target of parsing\narbitrary precision numbers coming from an untrusted source. The advantages\nover using 'Rational' for this are that:\n\n* A 'Scientific' is more efficient to construct. Rational numbers need to be\nconstructed using '%' which has to compute the 'gcd' of the 'numerator' and\n'denominator'.\n\n* 'Scientific' is safe against numbers with huge exponents. For example:\n@1e1000000000 :: 'Rational'@ will fill up all space and crash your\nprogram. Scientific works as expected:\n\n>>> read \"1e1000000000\" :: Scientific\n1.0e1000000000\n\n* Also, the space usage of converting scientific numbers with huge exponents to\n@'Integral's@ (like: 'Int') or @'RealFloat's@ (like: 'Double' or 'Float')\nwill always be bounded by the target type.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."integer-logarithms" or (errorHandler.buildDepError "integer-logarithms"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          ] ++ (if flags.bytestring-builder
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (if flags.integer-simple
          then [
            (hsPkgs."integer-simple" or (errorHandler.buildDepError "integer-simple"))
            ]
          else [
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            ]);
        buildable = true;
        };
      tests = {
        "test-scientific" = {
          depends = [
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-ant-xml" or (errorHandler.buildDepError "tasty-ant-xml"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (if flags.bytestring-builder
            then [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
              ]
            else [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ]);
          buildable = true;
          };
        };
      benchmarks = {
        "bench-scientific" = {
          depends = [
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/scientific-0.3.6.2.tar.gz";
      sha256 = "278d0afc87450254f8a76eab21b5583af63954efc9b74844a17a21a68013140f";
      });
    }) // {
    package-description-override = "name:                scientific\nversion:             0.3.6.2\nsynopsis:            Numbers represented using scientific notation\ndescription:\n  \"Data.Scientific\" provides the number type 'Scientific'. Scientific numbers are\n  arbitrary precision and space efficient. They are represented using\n  <http://en.wikipedia.org/wiki/Scientific_notation scientific notation>.\n  The implementation uses a coefficient @c :: 'Integer'@ and a base-10 exponent\n  @e :: 'Int'@. A scientific number corresponds to the\n  'Fractional' number: @'fromInteger' c * 10 '^^' e@.\n  .\n  Note that since we're using an 'Int' to represent the exponent these numbers\n  aren't truly arbitrary precision. I intend to change the type of the exponent\n  to 'Integer' in a future release.\n  .\n  The main application of 'Scientific' is to be used as the target of parsing\n  arbitrary precision numbers coming from an untrusted source. The advantages\n  over using 'Rational' for this are that:\n  .\n  * A 'Scientific' is more efficient to construct. Rational numbers need to be\n  constructed using '%' which has to compute the 'gcd' of the 'numerator' and\n  'denominator'.\n  .\n  * 'Scientific' is safe against numbers with huge exponents. For example:\n  @1e1000000000 :: 'Rational'@ will fill up all space and crash your\n  program. Scientific works as expected:\n  .\n  >>> read \"1e1000000000\" :: Scientific\n  1.0e1000000000\n  .\n  * Also, the space usage of converting scientific numbers with huge exponents to\n  @'Integral's@ (like: 'Int') or @'RealFloat's@ (like: 'Double' or 'Float')\n  will always be bounded by the target type.\n\nhomepage:            https://github.com/basvandijk/scientific\nbug-reports:         https://github.com/basvandijk/scientific/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Bas van Dijk\nmaintainer:          Bas van Dijk <v.dijk.bas@gmail.com>\ncategory:            Data\nbuild-type:          Simple\ncabal-version:       >=1.10\n\nextra-source-files:\n  changelog\n\nTested-With: GHC == 7.6.3\n           , GHC == 7.8.4\n           , GHC == 7.10.3\n           , GHC == 8.0.2\n           , GHC == 8.2.2\n           , GHC == 8.4.1\n\nsource-repository head\n  type:     git\n  location: git://github.com/basvandijk/scientific.git\n\nflag bytestring-builder\n  description: Depend on the bytestring-builder package for backwards compatibility.\n  default:     False\n  manual:      False\n\nflag integer-simple\n  description: Use the integer-simple package instead of integer-gmp\n  default:     False\n\nlibrary\n  exposed-modules:     Data.ByteString.Builder.Scientific\n                       Data.Scientific\n                       Data.Text.Lazy.Builder.Scientific\n  other-modules:       GHC.Integer.Compat\n                       Utils\n  other-extensions:    DeriveDataTypeable, BangPatterns\n  ghc-options:         -Wall\n  build-depends:       base        >= 4.3 && < 5\n                     , integer-logarithms >= 1\n                     , deepseq     >= 1.3\n                     , text        >= 0.8\n                     , hashable    >= 1.1.2\n                     , primitive   >= 0.1\n                     , containers  >= 0.1\n                     , binary      >= 0.4.1\n\n  if flag(bytestring-builder)\n      build-depends: bytestring         >= 0.9    && < 0.10.4\n                   , bytestring-builder >= 0.10.4 && < 0.11\n  else\n      build-depends: bytestring         >= 0.10.4\n\n  if flag(integer-simple)\n      build-depends: integer-simple\n  else\n      build-depends: integer-gmp\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n\ntest-suite test-scientific\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   test\n  main-is:          test.hs\n  default-language: Haskell2010\n  ghc-options:      -Wall\n\n  build-depends: scientific\n               , base             >= 4.3 && < 5\n               , binary           >= 0.4.1\n               , tasty            >= 0.5\n               , tasty-ant-xml    >= 1.0\n               , tasty-hunit      >= 0.8\n               , tasty-smallcheck >= 0.2\n               , tasty-quickcheck >= 0.8\n               , smallcheck       >= 1.0\n               , QuickCheck       >= 2.5\n               , text             >= 0.8\n\n  if flag(bytestring-builder)\n      build-depends: bytestring         >= 0.9    && < 0.10.4\n                   , bytestring-builder >= 0.10.4 && < 0.11\n  else\n      build-depends: bytestring         >= 0.10.4\n\nbenchmark bench-scientific\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   bench\n  main-is:          bench.hs\n  default-language: Haskell2010\n  ghc-options:      -O2\n  build-depends:    scientific\n                  , base        >= 4.3 && < 5\n                  , criterion   >= 0.5\n";
    }