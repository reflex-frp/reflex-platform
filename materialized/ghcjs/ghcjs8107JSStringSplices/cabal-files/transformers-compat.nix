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
      two = false;
      three = false;
      four = false;
      five = false;
      five-three = false;
      mtl = true;
      generic-deriving = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "transformers-compat"; version = "0.6.6"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2012-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/transformers-compat/";
      url = "";
      synopsis = "A small compatibility shim for the transformers library";
      description = "This package includes backported versions of types that were added\nto transformers in transformers 0.3, 0.4, and 0.5 for users who need strict\ntransformers 0.2 or 0.3 compatibility to run on old versions of the\nplatform, but also need those types.\n\nThose users should be able to just depend on @transformers >= 0.2@\nand @transformers-compat >= 0.3@.\n\nNote: missing methods are not supplied, but this at least permits the types to be used.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ (if flags.three
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (flags.mtl) (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          else [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ])) ++ (if flags.two
          then [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (flags.mtl) (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          else [
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" || flags.generic-deriving) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (flags.generic-deriving) ((pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0" && flags.generic-deriving) (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/transformers-compat-0.6.6.tar.gz";
      sha256 = "7e2e0251e5e6d28142615a4b950a3fabac9c0b7804b1ec4a4ae985f19519a9f9";
      });
    }) // {
    package-description-override = "name:          transformers-compat\ncategory:      Compatibility\nversion:       0.6.6\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/transformers-compat/\nbug-reports:   http://github.com/ekmett/transformers-compat/issues\ncopyright:     Copyright (C) 2012-2015 Edward A. Kmett\nsynopsis:      A small compatibility shim for the transformers library\ndescription:\n  This package includes backported versions of types that were added\n  to transformers in transformers 0.3, 0.4, and 0.5 for users who need strict\n  transformers 0.2 or 0.3 compatibility to run on old versions of the\n  platform, but also need those types.\n  .\n  Those users should be able to just depend on @transformers >= 0.2@\n  and @transformers-compat >= 0.3@.\n  .\n  Note: missing methods are not supplied, but this at least permits the types to be used.\n\nbuild-type:    Simple\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\nextra-source-files:\n  .travis.yml\n  .ghci\n  .gitignore\n  .hlint.yaml\n  .vim.custom\n  config\n  tests/*.hs\n  tests/LICENSE\n  tests/transformers-compat-tests.cabal\n  README.markdown\n  CHANGELOG.markdown\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/transformers-compat.git\n\nflag two\n  default: False\n  description: Use transformers 0.2. This will be selected by cabal picking the appropriate version.\n  manual: False\n\nflag three\n  default: False\n  manual: False\n  description: Use transformers 0.3. This will be selected by cabal picking the appropriate version.\n\nflag four\n  default: False\n  manual: False\n  description: Use transformers 0.4. This will be selected by cabal picking the appropriate version.\n\nflag five\n  default: False\n  manual: False\n  description: Use transformers 0.5 up until (but not including) 0.5.3. This will be selected by cabal picking the appropriate version.\n\nflag five-three\n  default: False\n  manual: False\n  description: Use transformers 0.5.3. This will be selected by cabal picking the appropriate version.\n\nflag mtl\n  default: True\n  manual: True\n  description: -f-mtl Disables support for mtl for transformers 0.2 and 0.3. That is an unsupported configuration, and results in missing instances for `ExceptT`.\n\nflag generic-deriving\n  default: True\n  manual: True\n  description: -f-generic-deriving prevents generic-deriving from being built as a dependency.\n               This disables certain aspects of generics for older versions of GHC. In particular,\n               Generic(1) instances will not be backported prior to GHC 7.2, and generic operations\n               over unlifted types will not be backported prior to GHC 8.0. This is an unsupported\n               configuration.\n\nlibrary\n  build-depends:\n    base >= 4.3 && < 5,\n    -- These are all transformers versions we support.\n    -- each flag below splits this interval into two parts.\n    -- flag-true parts are mutually exclusive, so at least one have to be on.\n    transformers >= 0.2 && <0.6\n  if !impl(ghc >= 8.0)\n    build-depends: fail == 4.9.*\n\n  hs-source-dirs:\n    src\n\n  exposed-modules:\n    Control.Monad.Trans.Instances\n\n  other-modules:\n    Paths_transformers_compat\n\n  default-language:\n    Haskell2010\n\n  -- automatic flags\n  if flag(five-three)\n    build-depends: transformers >= 0.5.3\n  else\n    build-depends: transformers < 0.5.3\n\n  if flag(five)\n    hs-source-dirs: 0.5\n    build-depends: transformers >= 0.5 && < 0.5.3\n  else\n    build-depends: transformers < 0.5 || >= 0.5.3\n\n  if flag(four)\n    cpp-options: -DTRANSFORMERS_FOUR\n    hs-source-dirs: 0.5\n    -- Don't allow transformers-0.4.0.0\n    -- See https://github.com/ekmett/transformers-compat/issues/35\n    build-depends: transformers >= 0.4.1 && < 0.5\n  else\n    build-depends: transformers < 0.4 || >= 0.5\n\n  if flag(three)\n    hs-source-dirs: 0.3 0.5\n    build-depends: transformers >= 0.3 && < 0.4\n    if flag(mtl)\n      build-depends: mtl >= 2.1 && < 2.2\n  else\n    build-depends: transformers < 0.3 || >= 0.4\n\n  if flag(two)\n    hs-source-dirs: 0.2 0.3 0.5\n    build-depends: transformers >= 0.2 && < 0.3\n    if flag(mtl)\n      build-depends: mtl >= 2.0 && < 2.1\n  else\n    build-depends: transformers >= 0.3\n\n  -- other flags\n  if impl(ghc >= 7.2) || flag(generic-deriving)\n    hs-source-dirs: generics\n    build-depends: ghc-prim\n\n  if flag(mtl)\n    cpp-options: -DMTL\n\n  if flag(generic-deriving)\n    if impl(ghc < 8.0) && flag(generic-deriving)\n      cpp-options: -DGENERIC_DERIVING\n      build-depends: generic-deriving >= 1.10 && < 2\n\n  if !flag(mtl) && !flag(generic-deriving)\n    cpp-options: -DHASKELL98\n\n  if flag(two)\n    exposed-modules:\n      Control.Applicative.Backwards\n      Control.Applicative.Lift\n      Data.Functor.Reverse\n\n  if flag(two) || flag(three)\n    exposed-modules:\n      Control.Monad.Trans.Except\n      Control.Monad.Signatures\n      Data.Functor.Classes\n      Data.Functor.Sum\n\n  if flag(two) || flag(three) || flag(four) || flag(five)\n    exposed-modules:\n      Control.Monad.Trans.Accum\n      Control.Monad.Trans.Select\n\n  if impl(ghc >= 7.2) || flag(generic-deriving)\n    exposed-modules:\n      Data.Functor.Classes.Generic\n      Data.Functor.Classes.Generic.Internal\n";
    }