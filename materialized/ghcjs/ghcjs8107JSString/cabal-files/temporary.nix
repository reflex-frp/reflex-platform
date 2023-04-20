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
      identifier = { name = "temporary"; version = "1.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mateusz Kowalczyk <fuuzetsu@fuuzetsu.co.uk>, Roman Cheplyaka <roma@ro-che.info>";
      author = "";
      homepage = "https://github.com/feuerbach/temporary";
      url = "";
      synopsis = "Portable temporary file and directory support";
      description = "Functions for creating temporary files and directories.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/temporary-1.3.tar.gz";
      sha256 = "8c442993694b5ffca823ce864af95bd2841fb5264ee511c61cf48cc71d879890";
      });
    }) // {
    package-description-override = "name:                temporary\nversion:             1.3\ncabal-version:       >= 1.10\nsynopsis:            Portable temporary file and directory support\ndescription:         Functions for creating temporary files and directories.\ncategory:            System, Utils\nlicense:             BSD3\nlicense-file:        LICENSE\nmaintainer:          Mateusz Kowalczyk <fuuzetsu@fuuzetsu.co.uk>, Roman Cheplyaka <roma@ro-che.info>\nhomepage:            https://github.com/feuerbach/temporary\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: git://github.com/feuerbach/temporary.git\n\nLibrary\n    default-language:\n      Haskell2010\n    exposed-modules: System.IO.Temp\n    build-depends:   base >= 3 && < 10, filepath >= 1.1, directory >= 1.0,\n                     transformers >= 0.2.0.0, exceptions >= 0.6, random >= 1.1\n                     -- note: the transformers dependency is needed for MonadIO\n                     -- on older GHCs; on newer ones, it is included in base.\n    ghc-options:     -Wall\n    \n    if !os(windows)\n        build-depends: unix >= 2.3\n\ntest-suite test\n  default-language:\n    Haskell2010\n  type:\n    exitcode-stdio-1.0\n  hs-source-dirs:\n    tests\n  main-is:\n    test.hs\n  ghc-options: -threaded -with-rtsopts=-N2\n  build-depends:\n      base >= 4.3 && < 5\n    , directory\n    , tasty\n    , tasty-hunit\n    , temporary\n    , filepath\n    , base-compat\n  if !os(windows)\n      build-depends: unix >= 2.3\n";
    }