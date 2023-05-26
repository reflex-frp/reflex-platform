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
      identifier = { name = "invariant"; version = "0.5.3"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Nicolas Frisby <nicolas.frisby@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      author = "Nicolas Frisby <nicolas.frisby@gmail.com>";
      homepage = "https://github.com/nfrisby/invariant-functors";
      url = "";
      synopsis = "Haskell98 invariant functors";
      description = "Haskell98 invariant functors (also known as exponential functors).\n\nFor more information, see Edward Kmett's article \\\"Rotten Bananas\\\":\n\n<http://comonad.com/reader/2008/rotten-bananas/>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."StateVar" or (errorHandler.buildDepError "StateVar"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/invariant-0.5.3.tar.gz";
      sha256 = "d73e5def38da9fdd85def073857aa5f4b1d3b0c2df05c43d58a677cca02d440c";
      });
    }) // {
    package-description-override = "name:                invariant\r\nversion:             0.5.3\r\nx-revision: 2\r\nsynopsis:            Haskell98 invariant functors\r\ndescription:         Haskell98 invariant functors (also known as exponential functors).\r\n                     .\r\n                     For more information, see Edward Kmett's article \\\"Rotten Bananas\\\":\r\n                     .\r\n                     <http://comonad.com/reader/2008/rotten-bananas/>\r\ncategory:            Control, Data\r\nlicense:             BSD2\r\nlicense-file:        LICENSE\r\nhomepage:            https://github.com/nfrisby/invariant-functors\r\nbug-reports:         https://github.com/nfrisby/invariant-functors/issues\r\nauthor:              Nicolas Frisby <nicolas.frisby@gmail.com>\r\nmaintainer:          Nicolas Frisby <nicolas.frisby@gmail.com>,\r\n                     Ryan Scott <ryan.gl.scott@gmail.com>\r\nbuild-type:          Simple\r\ncabal-version:       >= 1.9.2\r\ntested-with:         GHC == 7.0.4\r\n                   , GHC == 7.2.2\r\n                   , GHC == 7.4.2\r\n                   , GHC == 7.6.3\r\n                   , GHC == 7.8.4\r\n                   , GHC == 7.10.3\r\n                   , GHC == 8.0.2\r\n                   , GHC == 8.2.2\r\n                   , GHC == 8.4.4\r\n                   , GHC == 8.6.5\r\n                   , GHC == 8.8.1\r\nextra-source-files:  CHANGELOG.md, README.md\r\n\r\nsource-repository head\r\n  type:                git\r\n  location:            https://github.com/nfrisby/invariant-functors\r\n\r\nlibrary\r\n  exposed-modules:     Data.Functor.Invariant\r\n                     , Data.Functor.Invariant.TH\r\n  other-modules:       Data.Functor.Invariant.TH.Internal\r\n                     , Paths_invariant\r\n  hs-source-dirs:      src\r\n  build-depends:       array                >= 0.3    && < 0.6\r\n                     , base                 >= 4      && < 5\r\n                     , bifunctors           >= 5.2    && < 6\r\n                     , comonad              >= 5      && < 6\r\n                     , containers           >= 0.1    && < 0.7\r\n                     , contravariant        >= 0.5    && < 2\r\n                     , ghc-prim\r\n                     , profunctors          >= 5.2.1  && < 6\r\n                     , StateVar             >= 1.1    && < 2\r\n                     , stm                  >= 2.2    && < 3\r\n                     , tagged               >= 0.7.3  && < 1\r\n                     , template-haskell     >= 2.4    && < 2.17\r\n                     , th-abstraction       >= 0.2.2  && < 0.5\r\n                     , transformers         >= 0.2    && < 0.6\r\n                     , transformers-compat  >= 0.3    && < 1\r\n                     , unordered-containers >= 0.2.4  && < 0.3\r\n  ghc-options:         -Wall\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:     semigroups           >= 0.16.2 && < 1\r\n\r\ntest-suite spec\r\n  type:                exitcode-stdio-1.0\r\n  hs-source-dirs:      test\r\n  main-is:             Spec.hs\r\n  other-modules:       InvariantSpec\r\n                       THSpec\r\n  build-depends:       base             >= 4    && < 5\r\n                     , hspec            >= 1.8\r\n                     , invariant\r\n                     , QuickCheck       >= 2.11 && < 3\r\n                     , template-haskell >= 2.4  && < 2.17\r\n  build-tool-depends:  hspec-discover:hspec-discover\r\n  ghc-options:         -Wall\r\n";
    }