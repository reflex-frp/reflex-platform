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
      identifier = { name = "th-compat"; version = "0.1.4"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2020 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ryan Scott";
      homepage = "https://github.com/haskell-compat/th-compat";
      url = "";
      synopsis = "Backward- (and forward-)compatible Quote and Code types";
      description = "This package defines a \"Language.Haskell.TH.Syntax.Compat\"\nmodule, which backports the @Quote@ and @Code@ types to\nwork across a wide range of @template-haskell@ versions.\nThe @makeRelativeToProject@ utility is also backported.\nOn recent versions of @template-haskell@ (2.17.0.0 or\nlater), this module simply reexports definitions\nfrom \"Language.Haskell.TH.Syntax\". Refer to the Haddocks\nfor \"Language.Haskell.TH.Syntax.Compat\" for examples of\nhow to use this module.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "9.4")) [
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
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
      url = "http://hackage.haskell.org/package/th-compat-0.1.4.tar.gz";
      sha256 = "d8f97ac14ab47b6b8a7b0fdb4ff95426322ec56badd01652ac15da4a44d4bab8";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\nname:                th-compat\nversion:             0.1.4\nsynopsis:            Backward- (and forward-)compatible Quote and Code types\ndescription:         This package defines a \"Language.Haskell.TH.Syntax.Compat\"\n                     module, which backports the @Quote@ and @Code@ types to\n                     work across a wide range of @template-haskell@ versions.\n                     The @makeRelativeToProject@ utility is also backported.\n                     On recent versions of @template-haskell@ (2.17.0.0 or\n                     later), this module simply reexports definitions\n                     from \"Language.Haskell.TH.Syntax\". Refer to the Haddocks\n                     for \"Language.Haskell.TH.Syntax.Compat\" for examples of\n                     how to use this module.\nhomepage:            https://github.com/haskell-compat/th-compat\nbug-reports:         https://github.com/haskell-compat/th-compat/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Ryan Scott\nmaintainer:          Ryan Scott <ryan.gl.scott@gmail.com>\ncopyright:           (C) 2020 Ryan Scott\ncategory:            Text\nbuild-type:          Simple\ntested-with:         GHC == 7.0.4\n                   , GHC == 7.2.2\n                   , GHC == 7.4.2\n                   , GHC == 7.6.3\n                   , GHC == 7.8.4\n                   , GHC == 7.10.3\n                   , GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.7\n                   , GHC == 9.0.2\n                   , GHC == 9.2.3\n                   , GHC == 9.4.1\nextra-source-files:  CHANGELOG.md, README.md\n\nsource-repository head\n  type:                git\n  location:            https://github.com/haskell-compat/th-compat\n\nlibrary\n  exposed-modules:     Language.Haskell.TH.Syntax.Compat\n  build-depends:       base             >= 4.3 && < 5\n                     , template-haskell >= 2.5 && < 2.20\n  if !impl(ghc >= 8.0)\n    build-depends:     fail             == 4.9.*\n                     , transformers     >= 0.2 && < 0.7\n  if !impl(ghc >= 9.4)\n    build-depends:     filepath         >= 1.2.0.0 && < 1.5\n                     , directory        >= 1.1.0.0 && < 1.4\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  if impl(ghc >= 8.6)\n    ghc-options:       -Wno-star-is-type\n\ntest-suite spec\n  type:                exitcode-stdio-1.0\n  main-is:             Spec.hs\n  other-modules:       Language.Haskell.TH.Syntax.CompatSpec\n                       Types\n  build-depends:       base             >= 4.3 && < 5\n                     , base-compat      >= 0.6 && < 0.13\n                     , hspec            >= 2   && < 3\n                     , mtl              >= 2.1 && < 2.4\n                     , template-haskell >= 2.5 && < 2.20\n                     , th-compat\n  build-tool-depends:  hspec-discover:hspec-discover >= 2\n  hs-source-dirs:      tests\n  default-language:    Haskell2010\n  ghc-options:         -Wall -threaded -rtsopts\n";
    }