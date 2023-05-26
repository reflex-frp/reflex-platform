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
      containers = true;
      distributive = true;
      indexed-traversable = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "comonad"; version = "5.0.8"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2014 Edward A. Kmett,\nCopyright (C) 2004-2008 Dave Menendez";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/comonad/";
      url = "";
      synopsis = "Comonads";
      description = "Comonads.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.containers) (hsPkgs."containers" or (errorHandler.buildDepError "containers"))) ++ (pkgs.lib).optional (flags.distributive) (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))) ++ (pkgs.lib).optional (flags.indexed-traversable) (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/comonad-5.0.8.tar.gz";
      sha256 = "ef6cdf2cc292cc43ee6aa96c581b235fdea8ab44a0bffb24dc79ae2b2ef33d13";
      });
    }) // {
    package-description-override = "name:          comonad\ncategory:      Control, Comonads\nversion:       5.0.8\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/comonad/\nbug-reports:   http://github.com/ekmett/comonad/issues\ncopyright:     Copyright (C) 2008-2014 Edward A. Kmett,\n               Copyright (C) 2004-2008 Dave Menendez\nsynopsis:      Comonads\ndescription:   Comonads.\nbuild-type:    Simple\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\nextra-source-files:\n  .gitignore\n  .hlint.yaml\n  .vim.custom\n  coq/Store.v\n  README.markdown\n  CHANGELOG.markdown\n  examples/History.hs\n\nflag containers\n  description:\n    You can disable the use of the `containers` package using `-f-containers`.\n    .\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag distributive\n  description:\n    You can disable the use of the `distributive` package using `-f-distributive`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n    .\n    If disabled we will not supply instances of `Distributive`\n    .\n  default: True\n  manual: True\n\nflag indexed-traversable\n  description:\n    You can disable the use of the `indexed-traversable` package using `-f-indexed-traversable`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n    .\n    If disabled we will not supply instances of `FunctorWithIndex`\n    .\n  default: True\n  manual: True\n\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/comonad.git\n\nlibrary\n  hs-source-dirs: src\n  default-language: Haskell2010\n  ghc-options: -Wall\n\n  build-depends:\n    base                >= 4   && < 5,\n    tagged              >= 0.8.6.1 && < 1,\n    transformers        >= 0.3 && < 0.6,\n    transformers-compat >= 0.5 && < 1\n\n  if !impl(ghc >= 8.0)\n    build-depends: semigroups >= 0.18.5 && < 1\n\n  if flag(containers)\n    build-depends: containers >= 0.3 && < 0.7\n\n  if flag(distributive)\n    build-depends: distributive >= 0.5.2 && < 1\n\n  if flag(indexed-traversable)\n    build-depends: indexed-traversable >= 0.1.1 && < 0.2\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\n  exposed-modules:\n    Control.Comonad\n    Control.Comonad.Env\n    Control.Comonad.Env.Class\n    Control.Comonad.Hoist.Class\n    Control.Comonad.Identity\n    Control.Comonad.Store\n    Control.Comonad.Store.Class\n    Control.Comonad.Traced\n    Control.Comonad.Traced.Class\n    Control.Comonad.Trans.Class\n    Control.Comonad.Trans.Env\n    Control.Comonad.Trans.Identity\n    Control.Comonad.Trans.Store\n    Control.Comonad.Trans.Traced\n    Data.Functor.Composition\n\n  other-extensions:\n    CPP\n    RankNTypes\n    MultiParamTypeClasses\n    FunctionalDependencies\n    FlexibleInstances\n    UndecidableInstances\n";
    }