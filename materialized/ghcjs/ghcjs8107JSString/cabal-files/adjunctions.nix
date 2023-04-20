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
      identifier = { name = "adjunctions"; version = "4.4"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2014 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/adjunctions/";
      url = "";
      synopsis = "Adjunctions and representable functors";
      description = "Adjunctions and representable functors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
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
      url = "http://hackage.haskell.org/package/adjunctions-4.4.tar.gz";
      sha256 = "507c2ef55337ae61c805f8cbc1213dfd7d2b85187342675d662254b8d8a16ae9";
      });
    }) // {
    package-description-override = "name:          adjunctions\r\ncategory:      Data Structures, Adjunctions\r\nversion:       4.4\r\nx-revision: 2\r\nlicense:       BSD3\r\ncabal-version: >= 1.8\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/adjunctions/\r\nbug-reports:   http://github.com/ekmett/adjunctions/issues\r\ncopyright:     Copyright (C) 2011-2014 Edward A. Kmett\r\nsynopsis:      Adjunctions and representable functors\r\ndescription:   Adjunctions and representable functors.\r\nbuild-type:    Simple\r\nextra-source-files:\r\n  .gitignore\r\n  .travis.yml\r\n  .vim.custom\r\n  travis/cabal-apt-install\r\n  travis/config\r\n  HLint.hs\r\n  CHANGELOG.markdown\r\n  README.markdown\r\ntested-with:   GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.1\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/adjunctions.git\r\n\r\nlibrary\r\n  hs-source-dirs: src\r\n\r\n  other-extensions:\r\n    CPP\r\n    FunctionalDependencies\r\n    FlexibleContexts\r\n    MultiParamTypeClasses\r\n    Rank2Types\r\n    UndecidableInstances\r\n    DefaultSignatures\r\n\r\n  build-depends:\r\n    array               >= 0.3.0.2 && < 0.7,\r\n    base                >= 4       && < 5,\r\n    comonad             >= 4       && < 6,\r\n    containers          >= 0.3     && < 0.7,\r\n    contravariant       >= 1       && < 2,\r\n    distributive        >= 0.5.1   && < 1,\r\n    free                >= 4       && < 6,\r\n    mtl                 >= 2.0.1   && < 2.3,\r\n    profunctors         >= 4       && < 6,\r\n    tagged              >= 0.7     && < 1,\r\n    semigroupoids       >= 4       && < 6,\r\n    semigroups          >= 0.11    && < 1,\r\n    transformers        >= 0.2     && < 0.6,\r\n    transformers-compat >= 0.3     && < 1,\r\n    void                >= 0.5.5.1 && < 1\r\n\r\n  if impl(ghc < 7.6)\r\n    build-depends: ghc-prim\r\n\r\n  exposed-modules:\r\n    Control.Comonad.Representable.Store\r\n    Control.Comonad.Trans.Adjoint\r\n    Control.Monad.Representable.Reader\r\n    Control.Monad.Representable.State\r\n    Control.Monad.Trans.Adjoint\r\n    Control.Monad.Trans.Contravariant.Adjoint\r\n    Control.Monad.Trans.Conts\r\n    Data.Functor.Adjunction\r\n    Data.Functor.Contravariant.Adjunction\r\n    Data.Functor.Contravariant.Rep\r\n    Data.Functor.Rep\r\n\r\n  ghc-options: -Wall\r\n\r\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\r\n  if impl(ghc >= 8.0)\r\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances\r\n\r\ntest-suite spec\r\n  type:           exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n\r\n  build-tool-depends: hspec-discover:hspec-discover >=2 && <3\r\n  build-depends:\r\n    adjunctions,\r\n    base             >= 4     && < 5,\r\n    distributive     >= 0.5.1 && < 1,\r\n    generic-deriving >= 1.11  && < 2,\r\n    hspec            >= 2     && < 3\r\n\r\n  main-is: Spec.hs\r\n  other-modules: GenericsSpec\r\n\r\n  ghc-options: -Wall -threaded -rtsopts\r\n";
    }