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
      identifier = { name = "kan-extensions"; version = "5.2.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/kan-extensions/";
      url = "";
      synopsis = "Kan extensions, Kan lifts, the Yoneda lemma, and (co)density (co)monads";
      description = "Kan extensions, Kan lifts, various forms of the Yoneda lemma, and (co)density (co)monads.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/kan-extensions-5.2.2.tar.gz";
      sha256 = "3bf3ce4cacf9c57c03e9a1c36ecb1baf5d8356730853a2592d2112d1248498a0";
      });
    }) // {
    package-description-override = "name:          kan-extensions\ncategory:      Data Structures, Monads, Comonads, Functors\nversion:       5.2.2\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/kan-extensions/\nbug-reports:   http://github.com/ekmett/kan-extensions/issues\ncopyright:     Copyright (C) 2008-2016 Edward A. Kmett\nsynopsis:      Kan extensions, Kan lifts, the Yoneda lemma, and (co)density (co)monads\ndescription:   Kan extensions, Kan lifts, various forms of the Yoneda lemma, and (co)density (co)monads.\nbuild-type:    Simple\ntested-with:   GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\n\nextra-source-files:\n  .gitignore\n  .ghci\n  .vim.custom\n  CHANGELOG.markdown\n  README.markdown\n  include/kan-extensions-common.h\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/kan-extensions.git\n\nlibrary\n  hs-source-dirs: src\n  include-dirs: include\n  includes: kan-extensions-common.h\n\n  other-extensions:\n    CPP\n    MultiParamTypeClasses\n    GADTs\n    Rank2Types,\n    FlexibleInstances\n    FlexibleContexts\n    UndecidableInstances\n    TypeFamilies\n\n  build-depends:\n    adjunctions         >= 4.2     && < 5,\n    array               >= 0.3.0.2 && < 0.6,\n    base                >= 4.4     && < 5,\n    comonad             >= 4       && < 6,\n    containers          >= 0.4     && < 0.7,\n    contravariant       >= 1       && < 2,\n    distributive        >= 0.2.2   && < 1,\n    invariant           >= 0.1     && < 1,\n    free                >= 4       && < 6,\n    mtl                 >= 2.0.1   && < 2.3,\n    profunctors         >= 5       && < 6,\n    semigroupoids       >= 4       && < 6,\n    tagged              >= 0.7.2   && < 1,\n    transformers        >= 0.2     && < 0.6,\n    transformers-compat >= 0.3     && < 0.7\n\n  exposed-modules:\n    Control.Comonad.Density\n    Control.Monad.Co\n    Control.Monad.Codensity\n    Data.Functor.Contravariant.Day\n    Data.Functor.Contravariant.Yoneda\n    Data.Functor.Contravariant.Coyoneda\n    Data.Functor.Day\n    Data.Functor.Day.Curried\n    Data.Functor.Invariant.Day\n    Data.Functor.Kan.Lan\n    Data.Functor.Kan.Ran\n    Data.Functor.Yoneda\n    Data.Functor.Coyoneda\n\n  ghc-options: -Wall\n  default-language: Haskell2010\n\n  if impl(ghc >= 7.10)\n    ghc-options: -fno-warn-trustworthy-safe\n\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n\n    if !impl(ghc >= 8.8)\n      ghc-options: -Wnoncanonical-monadfail-instances\n  else\n    build-depends: fail >= 4.9 && < 5\n";
    }