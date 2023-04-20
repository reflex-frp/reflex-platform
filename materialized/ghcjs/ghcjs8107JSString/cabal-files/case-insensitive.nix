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
      specVersion = "1.8";
      identifier = { name = "case-insensitive"; version = "1.2.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2011 Bas van Dijk";
      maintainer = "Bas van Dijk <v.dijk.bas@gmail.com>";
      author = "Bas van Dijk";
      homepage = "https://github.com/basvandijk/case-insensitive";
      url = "";
      synopsis = "Case insensitive string comparison";
      description = "The module @Data.CaseInsensitive@ provides the 'CI' type\nconstructor which can be parameterised by a string-like\ntype like: 'String', 'ByteString', 'Text',\netc.. Comparisons of values of the resulting type will be\ninsensitive to cases.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test-case-insensitive" = {
          depends = [
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-case-insensitive" = {
          depends = [
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/case-insensitive-1.2.1.0.tar.gz";
      sha256 = "296dc17e0c5f3dfb3d82ced83e4c9c44c338ecde749b278b6eae512f1d04e406";
      });
    }) // {
    package-description-override = "name:          case-insensitive\nversion:       1.2.1.0\ncabal-version: >=1.8\nbuild-type:    Simple\nlicense:       BSD3\nlicense-file:  LICENSE\ncopyright:     2011 Bas van Dijk\nauthor:        Bas van Dijk\nmaintainer:    Bas van Dijk <v.dijk.bas@gmail.com>\nhomepage:      https://github.com/basvandijk/case-insensitive\nbug-reports:   https://github.com/basvandijk/case-insensitive/issues\ncategory:      Data, Text\nsynopsis:      Case insensitive string comparison\ndescription:   The module @Data.CaseInsensitive@ provides the 'CI' type\n               constructor which can be parameterised by a string-like\n               type like: 'String', 'ByteString', 'Text',\n               etc.. Comparisons of values of the resulting type will be\n               insensitive to cases.\ntested-with:\n  GHC==7.0.4,\n  GHC==7.2.2\n  GHC==7.4.2,\n  GHC==7.6.3,\n  GHC==7.8.4,\n  GHC==7.10.3,\n  GHC==8.0.1\n\nextra-source-files: README.markdown CHANGELOG pg2189.txt\n\nsource-repository head\n  Type:     git\n  Location: git://github.com/basvandijk/case-insensitive.git\n\nLibrary\n  ghc-options: -Wall\n  build-depends: base       >= 3 && < 5\n               , bytestring >= 0.9\n               , text       >= 0.3\n               , deepseq    >= 1.1\n               , hashable   >= 1.0\n  if !impl(ghc >= 8.0)\n    build-depends: semigroups >= 0.18\n  exposed-modules: Data.CaseInsensitive, Data.CaseInsensitive.Unsafe\n  other-modules: Data.CaseInsensitive.Internal\n\ntest-suite test-case-insensitive\n  type:           exitcode-stdio-1.0\n  main-is:        test.hs\n  hs-source-dirs: test\n\n  build-depends: case-insensitive\n               , base                 >= 3 && < 5\n               , bytestring           >= 0.9\n               , text                 >= 0.3\n               , HUnit                >= 1.2.2\n               , test-framework       >= 0.2.4\n               , test-framework-hunit >= 0.2.4\n\n  ghc-options: -Wall\n\nbenchmark bench-case-insensitive\n  type:           exitcode-stdio-1.0\n  main-is:        bench.hs\n  other-modules:  NoClass\n  hs-source-dirs: bench\n\n  ghc-options:    -Wall -O2\n\n  build-depends: case-insensitive\n               , base       >= 3 && < 5\n               , bytestring >= 0.9\n               , criterion  >= 0.6.1\n               , deepseq    >= 1.1\n";
    }