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
      identifier = { name = "StateVar"; version = "1.2.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2014-2015 Edward A. Kmett, 2009-2018 Sven Panne";
      maintainer = "Sven Panne <svenpanne@gmail.com>";
      author = "Sven Panne and Edward Kmett";
      homepage = "https://github.com/haskell-opengl/StateVar";
      url = "";
      synopsis = "State variables";
      description = "This package contains state variables, which are references in the IO monad,\nlike IORefs or parts of the OpenGL state.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/StateVar-1.2.1.tar.gz";
      sha256 = "ee261552912b60d8b937f0253615e310e6cc25f9c407001b3bcc2e3d55000f8b";
      });
    }) // {
    package-description-override = "name: StateVar\nversion: 1.2.1\nsynopsis: State variables\ndescription:\n  This package contains state variables, which are references in the IO monad,\n  like IORefs or parts of the OpenGL state.\nhomepage: https://github.com/haskell-opengl/StateVar\nbug-reports: https://github.com/haskell-opengl/StateVar/issues\ncopyright: Copyright (C) 2014-2015 Edward A. Kmett, 2009-2018 Sven Panne\nlicense: BSD3\nlicense-file: LICENSE\nauthor: Sven Panne and Edward Kmett\nmaintainer: Sven Panne <svenpanne@gmail.com>\ncategory: Data\nbuild-type: Simple\ntested-with:\n  GHC == 7.0.4\n  GHC == 7.2.2\n  GHC == 7.4.2\n  GHC == 7.6.3\n  GHC == 7.8.4\n  GHC == 7.10.3\n  GHC == 8.0.2\n  GHC == 8.2.2\n  GHC == 8.4.3\n  GHC == 8.6.5\n  GHC == 8.8.4\n  GHC == 8.10.3\ncabal-version: >= 1.10\nextra-source-files:\n  README.md\n  CHANGELOG.md\n\nlibrary\n  exposed-modules:\n    Data.StateVar\n\n  build-depends:\n    base         >= 4   && < 5,\n    stm          >= 2.3.0.1 && < 2.6,\n    transformers >= 0.3 && < 0.6\n\n  default-language: Haskell2010\n  other-extensions:\n    CPP\n    DeriveDataTypeable\n    MultiParamTypeClasses\n    FunctionalDependencies\n    FlexibleInstances\n    TypeFamilies\n\n  hs-source-dirs: src\n  ghc-options: -Wall\n  if impl(ghc > 8)\n    ghc-options: -Wcompat\n  if impl(ghc>=7.4)\n    -- other-extensions: DefaultSignatures\n    cpp-options: -DUSE_DEFAULT_SIGNATURES=1\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-opengl/StateVar.git\n";
    }