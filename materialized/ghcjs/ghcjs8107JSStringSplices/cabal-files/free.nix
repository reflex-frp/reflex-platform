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
      specVersion = "1.18";
      identifier = { name = "free"; version = "5.1.3"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/free/";
      url = "";
      synopsis = "Monads for free";
      description = "Free monads are useful for many tree-like structures and domain specific languages.\n\nIf @f@ is a 'Functor' then the free 'Monad' on @f@ is the type\nof trees whose nodes are labeled with the constructors of @f@. The word\n\\\"free\\\" is used in the sense of \\\"unrestricted\\\" rather than \\\"zero-cost\\\":\n@Free f@ makes no constraining assumptions beyond those given by @f@ and the\ndefinition of 'Monad'. As used here it is a standard term from the\nmathematical theory of adjoint functors.\n\nCofree comonads are dual to free monads. They provide convenient ways to talk\nabout branching streams and rose-trees, and can be used to annotate syntax\ntrees. The cofree comonad can be seen as a stream parameterized by a 'Functor'\nthat controls its branching factor.\n\nMore information on free monads, including examples, can be found in the\nfollowing blog posts:\n<http://comonad.com/reader/2008/monads-for-free/>\n<http://comonad.com/reader/2011/free-monads-for-less/>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if compiler.isGhc && (compiler.version).ge "7.10"
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]
          else [
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ])) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/free-5.1.3.tar.gz";
      sha256 = "2c70d66e3a1ad52ce4b22d5510ffc6d7b3db950bd7f43bc61801cfe7b24c2e2d";
      });
    }) // {
    package-description-override = "name:          free\ncategory:      Control, Monads\nversion:       5.1.3\nx-revision: 1\nlicense:       BSD3\ncabal-version: 1.18\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/free/\nbug-reports:   http://github.com/ekmett/free/issues\ncopyright:     Copyright (C) 2008-2015 Edward A. Kmett\ntested-with:   GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.1\n             , GHC == 8.10.1\nsynopsis:      Monads for free\ndescription:\n  Free monads are useful for many tree-like structures and domain specific languages.\n  .\n  If @f@ is a 'Functor' then the free 'Monad' on @f@ is the type\n  of trees whose nodes are labeled with the constructors of @f@. The word\n  \\\"free\\\" is used in the sense of \\\"unrestricted\\\" rather than \\\"zero-cost\\\":\n  @Free f@ makes no constraining assumptions beyond those given by @f@ and the\n  definition of 'Monad'. As used here it is a standard term from the\n  mathematical theory of adjoint functors.\n  .\n  Cofree comonads are dual to free monads. They provide convenient ways to talk\n  about branching streams and rose-trees, and can be used to annotate syntax\n  trees. The cofree comonad can be seen as a stream parameterized by a 'Functor'\n  that controls its branching factor.\n  .\n  More information on free monads, including examples, can be found in the\n  following blog posts:\n  <http://comonad.com/reader/2008/monads-for-free/>\n  <http://comonad.com/reader/2011/free-monads-for-less/>\n\nbuild-type:    Simple\nextra-source-files:\n  .ghci\n  .gitignore\n  .hlint.yaml\n  .travis.yml\n  .vim.custom\n  README.markdown\n  CHANGELOG.markdown\n  doc/proof/Control/Comonad/Cofree/*.md\n  doc/proof/Control/Comonad/Trans/Cofree/*.md\n  examples/free-examples.cabal\n  examples/LICENSE\n  examples/*.hs\n  examples/*.lhs\n  include/free-common.h\nextra-doc-files:\n  examples/*.hs\n  examples/*.lhs\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/free.git\n\nlibrary\n  hs-source-dirs: src\n  include-dirs: include\n  includes: free-common.h\n\n  default-language:   Haskell2010\n  default-extensions: CPP\n  other-extensions:\n    MultiParamTypeClasses\n    FunctionalDependencies\n    FlexibleInstances\n    UndecidableInstances\n    Rank2Types\n    GADTs\n\n  build-depends:\n    base                 == 4.*,\n    comonad              >= 4 && < 6,\n    distributive         >= 0.2.1,\n    mtl                  >= 2.0.1.0 && < 2.3,\n    profunctors          >= 4 && < 6,\n    semigroupoids        >= 4 && < 6,\n    transformers         >= 0.2.0   && < 0.6,\n    transformers-base    >= 0.4 && < 0.5,\n    template-haskell     >= 2.7.0.0 && < 2.17,\n    exceptions           >= 0.6 && < 0.11,\n    containers           < 0.7\n\n  if !impl(ghc >= 8.2)\n    build-depends: bifunctors >= 4 && < 6\n\n  if !impl(ghc >= 8.0)\n    build-depends: semigroups >= 0.8.3.1 && < 1\n\n  -- Ensure Data.Functor.Classes is always available\n  if impl(ghc >= 7.10)\n    build-depends: transformers >= 0.4.2.0\n  else\n    build-depends: transformers-compat >= 0.5.1.0 && <0.7\n\n  exposed-modules:\n    Control.Applicative.Free\n    Control.Applicative.Free.Fast\n    Control.Applicative.Free.Final\n    Control.Applicative.Trans.Free\n    Control.Alternative.Free\n    Control.Alternative.Free.Final\n    Control.Comonad.Cofree\n    Control.Comonad.Cofree.Class\n    Control.Comonad.Trans.Cofree\n    Control.Comonad.Trans.Coiter\n    Control.Monad.Free\n    Control.Monad.Free.Ap\n    Control.Monad.Free.Church\n    Control.Monad.Free.Class\n    Control.Monad.Free.TH\n    Control.Monad.Trans.Free\n    Control.Monad.Trans.Free.Ap\n    Control.Monad.Trans.Free.Church\n    Control.Monad.Trans.Iter\n\n  other-modules:\n    Data.Functor.Classes.Compat\n\n  ghc-options: -Wall\n\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances\n  else\n    build-depends: fail == 4.9.*\n";
    }