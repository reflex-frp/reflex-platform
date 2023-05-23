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
      identifier = { name = "enclosed-exceptions"; version = "1.0.3"; };
      license = "MIT";
      copyright = "";
      maintainer = "jmacristovao@gmail.com, michael@snoyman.com";
      author = "Michael Snoyman, João Cristóvão";
      homepage = "https://github.com/jcristovao/enclosed-exceptions";
      url = "";
      synopsis = "Catching all exceptions from within an enclosed computation";
      description = "Catching all exceptions raised within an enclosed computation,\nwhile remaining responsive to (external) asynchronous exceptions.\nFor more information on the technique, please see:\n<https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/enclosed-exceptions-1.0.3.tar.gz";
      sha256 = "af6d93f113ac92b89a32af1fed52f445f492afcc0be93980cbadc5698f94f0b9";
      });
    }) // {
    package-description-override = "name:                enclosed-exceptions\nversion:             1.0.3\nsynopsis:            Catching all exceptions from within an enclosed computation\ndescription:         Catching all exceptions raised within an enclosed computation,\n                     while remaining responsive to (external) asynchronous exceptions.\n                     For more information on the technique, please see:\n                     <https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions>\nhomepage:            https://github.com/jcristovao/enclosed-exceptions\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Michael Snoyman, João Cristóvão\nmaintainer:          jmacristovao@gmail.com, michael@snoyman.com\ncategory:            Control\nbuild-type:          Simple\ncabal-version:       >=1.8\nextra-source-files:  README.md ChangeLog.md\n\nlibrary\n  exposed-modules:   Control.Exception.Enclosed\n  hs-source-dirs:    src\n  build-depends:       base                          >= 4.6        && < 5\n                     , transformers\n                     , lifted-base                   >= 0.2\n                     , monad-control\n                     , deepseq\n                     , transformers-base\n  ghc-options:         -Wall -fno-warn-orphans\n\ntest-suite test\n    hs-source-dirs: src, test\n    main-is: main.hs\n    type: exitcode-stdio-1.0\n    build-depends:   base\n                   , lifted-base                   >= 0.2\n                   , monad-control\n                   , async                         >= 2.0\n                   , deepseq\n                   , hspec                         >= 1.3\n                   , QuickCheck\n                   , stm\n                   , transformers\n                   , transformers-base\n    ghc-options:     -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/jcristovao/enclosed-exceptions.git\n";
    }