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
      identifier = { name = "profunctors"; version = "5.6"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/profunctors/";
      url = "";
      synopsis = "Profunctors";
      description = "Profunctors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/profunctors-5.6.tar.gz";
      sha256 = "cb06a548f67c17d38fef7b2e5d1f66a5e48f353d7806290e795cc97c9a298ce3";
      });
    }) // {
    package-description-override = "name:          profunctors\ncategory:      Control, Categories\nversion:       5.6\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     experimental\nhomepage:      http://github.com/ekmett/profunctors/\nbug-reports:   http://github.com/ekmett/profunctors/issues\ncopyright:     Copyright (C) 2011-2015 Edward A. Kmett\nsynopsis:      Profunctors\ndescription:   Profunctors.\ntested-with:   GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\nbuild-type:    Simple\nextra-source-files:\n  .ghci\n  .gitignore\n  .hlint.yaml\n  .travis.yml\n  .vim.custom\n  README.markdown\n  CHANGELOG.markdown\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/profunctors.git\n\nlibrary\n  build-depends:\n    base                >= 4.7   && < 5,\n    base-orphans        >= 0.4   && < 0.9,\n    bifunctors          >= 5.2   && < 6,\n    comonad             >= 4     && < 6,\n    contravariant       >= 1     && < 2,\n    distributive        >= 0.4.4 && < 1,\n    tagged              >= 0.4.4 && < 1,\n    transformers        >= 0.2   && < 0.6\n\n  if !impl(ghc >= 8.0)\n    build-depends: semigroups >= 0.11 && < 0.20\n\n  exposed-modules:\n    Data.Profunctor\n    Data.Profunctor.Adjunction\n    Data.Profunctor.Cayley\n    Data.Profunctor.Choice\n    Data.Profunctor.Closed\n    Data.Profunctor.Composition\n    Data.Profunctor.Mapping\n    Data.Profunctor.Monad\n    Data.Profunctor.Ran\n    Data.Profunctor.Rep\n    Data.Profunctor.Sieve\n    Data.Profunctor.Strong\n    Data.Profunctor.Traversing\n    Data.Profunctor.Types\n    Data.Profunctor.Unsafe\n    Data.Profunctor.Yoneda\n\n  ghc-options:     -Wall -O2\n\n  if impl(ghc>=8.0)\n    ghc-options: -Wno-trustworthy-safe\n\n  if impl(ghc >= 8.6)\n    ghc-options: -Wno-star-is-type\n\n  hs-source-dirs:  src\n\n  default-language: Haskell2010\n  other-extensions:\n    CPP\n    GADTs\n    FlexibleContexts\n    FlexibleInstances\n    InstanceSigs\n    UndecidableInstances\n    TypeFamilies\n";
    }