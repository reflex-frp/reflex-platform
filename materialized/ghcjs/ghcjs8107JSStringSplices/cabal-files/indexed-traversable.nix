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
      specVersion = "1.12";
      identifier = { name = "indexed-traversable"; version = "0.1.1"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Edward Kmett";
      homepage = "";
      url = "";
      synopsis = "FunctorWithIndex, FoldableWithIndex, TraversableWithIndex";
      description = "This package provides three useful generalizations:\n\n@\nclass Functor f => FunctorWithIndex i f | f -> i where\n\\  imap :: (i -> a -> b) -> f a -> f b\n@\n\n@\nclass Foldable f => FoldableWithIndex i f | f -> i where\n\\  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m\n@\n\n@\nclass (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) => TraversableWithIndex i t | t -> i where\n\\  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)\n@\n\nThis package contains instances for types in GHC boot libraries.\nFor some additional instances see [indexed-traversable-instances](https://hackage.haskell.org/package/indexed-traversable-instances).\n\nThe [keys](https://hackage.haskell.org/package/keys) package provides similar functionality,\nbut uses (associated) @TypeFamilies@ instead of @FunctionalDependencies@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (((([
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."void" or (errorHandler.buildDepError "void"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.0" && (compiler.isGhc && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.0" && (compiler.isGhc && (compiler.version).lt "7.2")) (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/indexed-traversable-0.1.1.tar.gz";
      sha256 = "7ac36ae3153cbe7a8e99eacffd065367b87544953cc92997f424a150db468139";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               indexed-traversable\nversion:            0.1.1\nbuild-type:         Simple\nlicense:            BSD2\nlicense-file:       LICENSE\ncategory:           Data\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nauthor:             Edward Kmett\nsynopsis:           FunctorWithIndex, FoldableWithIndex, TraversableWithIndex\ndescription:\n  This package provides three useful generalizations:\n  .\n  @\n  class Functor f => FunctorWithIndex i f | f -> i where\n  \\  imap :: (i -> a -> b) -> f a -> f b\n  @\n  .\n  @\n  class Foldable f => FoldableWithIndex i f | f -> i where\n  \\  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m\n  @\n  .\n  @\n  class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) => TraversableWithIndex i t | t -> i where\n  \\  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)\n  @\n  .\n  This package contains instances for types in GHC boot libraries.\n  For some additional instances see [indexed-traversable-instances](https://hackage.haskell.org/package/indexed-traversable-instances).\n  .\n  The [keys](https://hackage.haskell.org/package/keys) package provides similar functionality,\n  but uses (associated) @TypeFamilies@ instead of @FunctionalDependencies@.\n\nextra-source-files: Changelog.md\ntested-with:\n  GHC ==7.0.4\n   || ==7.2.2\n   || ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.2\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/indexed-traversable\n  subdir:   indexed-traversable\n\nlibrary\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  hs-source-dirs:   src\n  other-modules:\n    GhcExts\n    WithIndex\n\n  exposed-modules:\n    Data.Foldable.WithIndex\n    Data.Functor.WithIndex\n    Data.Traversable.WithIndex\n\n  build-depends:\n      array         >=0.3.0.2 && <0.6\n    , base          >=4.3     && <4.16\n    , containers    >=0.4.0.0 && <0.7\n    , transformers  >=0.3.0.0 && <0.6\n\n  if !impl(ghc >=7.8)\n    build-depends: tagged >=0.8.5 && <0.9\n\n  if !impl(ghc >=7.10)\n    build-depends: void >=0.7.2 && <0.8\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        base-orphans         >=0.8.3  && <0.9\n      , semigroups           >=0.18.4 && <0.20\n      , transformers-compat  >=0.6.6  && <0.7\n\n  if (impl(ghc >=7.0) && impl(ghc <7.6))\n    build-depends: ghc-prim\n\n  if (impl(ghc >=7.0) && impl(ghc <7.2))\n    build-depends: generic-deriving ==1.14.*\n";
    }