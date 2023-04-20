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
    flags = {
      developer = false;
      fast = false;
      bytestring-builder = false;
      cffi = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "aeson"; version = "1.4.7.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011-2016 Bryan O'Sullivan\n(c) 2011 MailRank, Inc.";
      maintainer = "Adam Bergmark <adam@bergmark.nl>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/bos/aeson";
      url = "";
      synopsis = "Fast JSON parsing and encoding";
      description = "A JSON parsing and encoding library optimized for ease of use\nand high performance.\n\nTo get started, see the documentation for the @Data.Aeson@ module\nbelow.\n\n(A note on naming: in Greek mythology, Aeson was the father of Jason.)";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
          (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.0") (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.6")) (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = ((([
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."integer-logarithms" or (errorHandler.buildDepError "integer-logarithms"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            ] ++ (if flags.bytestring-builder
            then [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
              ]
            else [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ])) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
            (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.8") (hsPkgs."hashable-time" or (errorHandler.buildDepError "hashable-time"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/aeson-1.4.7.1.tar.gz";
      sha256 = "07e746655fd9bec81c59927c5617877ff4fcd81d0df45c5fb8ef154fb8f40294";
      });
    }) // {
    package-description-override = "name:            aeson\r\nversion:         1.4.7.1\r\nx-revision: 1\r\nlicense:         BSD3\r\nlicense-file:    LICENSE\r\ncategory:        Text, Web, JSON\r\ncopyright:       (c) 2011-2016 Bryan O'Sullivan\r\n                 (c) 2011 MailRank, Inc.\r\nauthor:          Bryan O'Sullivan <bos@serpentine.com>\r\nmaintainer:      Adam Bergmark <adam@bergmark.nl>\r\nstability:       experimental\r\ntested-with:     GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.1\r\nsynopsis:        Fast JSON parsing and encoding\r\ncabal-version:   >= 1.10\r\nhomepage:        https://github.com/bos/aeson\r\nbug-reports:     https://github.com/bos/aeson/issues\r\nbuild-type:      Simple\r\ndescription:\r\n    A JSON parsing and encoding library optimized for ease of use\r\n    and high performance.\r\n    .\r\n    To get started, see the documentation for the @Data.Aeson@ module\r\n    below.\r\n    .\r\n    (A note on naming: in Greek mythology, Aeson was the father of Jason.)\r\n\r\nextra-source-files:\r\n    *.yaml\r\n    README.markdown\r\n    benchmarks/*.cabal\r\n    benchmarks/*.hs\r\n    benchmarks/*.py\r\n    benchmarks/Compare/*.hs\r\n    benchmarks/Makefile\r\n    benchmarks/Typed/*.hs\r\n    benchmarks/json-data/*.json\r\n    cbits/*.c\r\n    changelog.md\r\n    examples/*.cabal\r\n    examples/*.hs\r\n    examples/Twitter/*.hs\r\n    ffi/Data/Aeson/Parser/*.hs\r\n    include/*.h\r\n    tests/JSONTestSuite/test_parsing/*.json\r\n    tests/JSONTestSuite/test_transform/*.json\r\n    tests/golden/*.expected\r\n    pure/Data/Aeson/Parser/*.hs\r\n\r\nflag developer\r\n  description: operate in developer mode\r\n  default: False\r\n  manual: True\r\n\r\nflag fast\r\n  description: compile without optimizations\r\n  default: False\r\n  manual: True\r\n\r\nflag bytestring-builder\r\n  description: Depend on the bytestring-builder package for backwards compatibility.\r\n  default: False\r\n  manual: False\r\n\r\nflag cffi\r\n  description: Controls whether to include c-ffi bits or pure haskell. Default to False for security.\r\n  default: False\r\n  manual: True\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  hs-source-dirs: . attoparsec-iso8601/\r\n\r\n  exposed-modules:\r\n    Data.Aeson\r\n    Data.Aeson.Encoding\r\n    Data.Aeson.Parser\r\n    Data.Aeson.Text\r\n    Data.Aeson.Types\r\n    Data.Aeson.TH\r\n    Data.Aeson.QQ.Simple\r\n\r\n    Data.Aeson.Encoding.Internal\r\n    Data.Aeson.Internal\r\n    Data.Aeson.Internal.Time\r\n    Data.Aeson.Parser.Internal\r\n\r\n  -- Deprecated modules\r\n  exposed-modules:\r\n    Data.Aeson.Encode\r\n\r\n  other-modules:\r\n    Data.Aeson.Encoding.Builder\r\n    Data.Aeson.Internal.Functions\r\n    Data.Aeson.Parser.Unescape\r\n    Data.Aeson.Parser.Time\r\n    Data.Aeson.Types.FromJSON\r\n    Data.Aeson.Types.Generic\r\n    Data.Aeson.Types.ToJSON\r\n    Data.Aeson.Types.Class\r\n    Data.Aeson.Types.Internal\r\n    Data.Attoparsec.Time\r\n    Data.Attoparsec.Time.Internal\r\n\r\n  -- GHC bundled libs\r\n  build-depends:\r\n    base             >= 4.7.0.0 && < 5,\r\n    bytestring       >= 0.10.4.0 && < 0.11,\r\n    containers       >= 0.5.5.1 && < 0.7,\r\n    deepseq          >= 1.3.0.0 && < 1.5,\r\n    ghc-prim         >= 0.2     && < 0.7,\r\n    template-haskell >= 2.9.0.0 && < 2.17,\r\n    text             >= 1.2.3.0 && < 1.3,\r\n    time             >= 1.4     && < 1.11\r\n\r\n  if impl(ghc >= 8.0)\r\n    build-depends: bytestring >= 0.10.8.1\r\n\r\n  -- Compat\r\n  build-depends:\r\n    base-compat-batteries >= 0.10.0   && < 0.12,\r\n    time-compat           >= 1.9.2.2  && < 1.10\r\n\r\n  if !impl(ghc >= 8.6)\r\n    build-depends:\r\n      contravariant >=1.4.1    && <1.6\r\n\r\n  if !impl(ghc >= 8.0)\r\n    -- `Data.Semigroup` and `Control.Monad.Fail` and `Control.Monad.IO.Class` are available in base only since GHC 8.0 / base 4.9\r\n    build-depends:\r\n      semigroups          >= 0.18.5  && < 0.20,\r\n      transformers        >= 0.3.0.0 && < 0.6,\r\n      transformers-compat >= 0.6.2   && < 0.7,\r\n      fail == 4.9.*\r\n\r\n  if !impl(ghc >= 7.10)\r\n    -- `Numeric.Natural` is available in base only since GHC 7.10 / base 4.8\r\n    build-depends: nats >= 1.1.1 && < 1.2,\r\n                   void >= 0.7.2 && < 0.8\r\n\r\n  -- cannot use latest version\r\n  build-depends:\r\n    unordered-containers >= 0.2.8.0  && < 0.3,\r\n\r\n    -- not in LTS-12.10\r\n    tagged               >= 0.8.5    && < 0.9,\r\n    primitive            >= 0.6.3.0  && < 0.8\r\n\r\n  -- Other dependencies\r\n  build-depends:\r\n    attoparsec           >= 0.13.2.2 && < 0.14,\r\n    dlist                >= 0.8.0.4  && < 0.9,\r\n    hashable             >= 1.2.7.0  && < 1.4,\r\n    scientific           >= 0.3.6.2  && < 0.4,\r\n    th-abstraction       >= 0.2.8.0  && < 0.4,\r\n    uuid-types           >= 1.0.3    && < 1.1,\r\n    vector               >= 0.12.0.1 && < 0.13\r\n\r\n  ghc-options: -Wall\r\n\r\n  if flag(developer)\r\n    ghc-options: -Werror\r\n    ghc-prof-options: -auto-all\r\n\r\n  if flag(fast)\r\n    ghc-options: -O0\r\n  else\r\n    ghc-options: -O2\r\n\r\n  include-dirs: include\r\n  if impl(ghcjs) || !flag(cffi)\r\n    hs-source-dirs: pure\r\n    other-modules: Data.Aeson.Parser.UnescapePure\r\n  else\r\n    c-sources: cbits/unescape_string.c\r\n    cpp-options: -DCFFI\r\n    hs-source-dirs: ffi\r\n    other-modules: Data.Aeson.Parser.UnescapeFFI\r\n\r\ntest-suite tests\r\n  default-language: Haskell2010\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: tests ffi pure\r\n  main-is: Tests.hs\r\n  c-sources: cbits/unescape_string.c\r\n  ghc-options: -Wall -threaded -rtsopts\r\n\r\n  other-modules:\r\n    Data.Aeson.Parser.UnescapeFFI\r\n    Data.Aeson.Parser.UnescapePure\r\n    DataFamilies.Properties\r\n    DataFamilies.Instances\r\n    DataFamilies.Encoders\r\n    DataFamilies.Types\r\n    Encoders\r\n    ErrorMessages\r\n    Functions\r\n    Instances\r\n    Options\r\n    PropUtils\r\n    Properties\r\n    PropertyGeneric\r\n    PropertyKeys\r\n    PropertyRoundTrip\r\n    PropertyRTFunctors\r\n    PropertyTH\r\n    SerializationFormatSpec\r\n    Types\r\n    UnitTests\r\n    UnitTests.NullaryConstructors\r\n\r\n  build-depends:\r\n    QuickCheck >= 2.10.0.1 && < 2.14,\r\n    aeson,\r\n    integer-logarithms >= 1 && <1.1,\r\n    attoparsec,\r\n    base,\r\n    base-compat,\r\n    base-orphans >= 0.5.3 && <0.9,\r\n    base16-bytestring,\r\n    containers,\r\n    directory,\r\n    dlist,\r\n    Diff >= 0.4 && < 0.5,\r\n    filepath,\r\n    generic-deriving >= 1.10 && < 1.14,\r\n    ghc-prim >= 0.2,\r\n    hashable >= 1.2.4.0,\r\n    scientific,\r\n    tagged,\r\n    template-haskell,\r\n    tasty,\r\n    tasty-golden,\r\n    tasty-hunit,\r\n    tasty-quickcheck,\r\n    text,\r\n    time,\r\n    time-compat,\r\n    unordered-containers,\r\n    uuid-types,\r\n    vector,\r\n    quickcheck-instances >= 0.3.21 && <0.4\r\n\r\n  if flag(bytestring-builder)\r\n    build-depends: bytestring >= 0.9 && < 0.10.4,\r\n                   bytestring-builder >= 0.10.4 && < 1\r\n  else\r\n    build-depends: bytestring >= 0.10.4\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:\r\n      semigroups >= 0.18.2 && < 0.20,\r\n      transformers >= 0.2.2.0,\r\n      transformers-compat >= 0.3\r\n\r\n  if !impl(ghc >= 7.10)\r\n    build-depends: nats >=1 && <1.2,\r\n                   void >=0.7.2 && <0.8\r\n\r\n  if impl(ghc >= 7.8)\r\n    build-depends: hashable-time >= 0.2 && <0.3\r\n\r\n  if flag(fast)\r\n    ghc-options: -fno-enable-rewrite-rules\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/bos/aeson.git\r\n";
    }