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
      identifier = { name = "file-embed"; version = "0.0.13.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "https://github.com/snoyberg/file-embed";
      url = "";
      synopsis = "Use Template Haskell to embed file contents directly.";
      description = "Use Template Haskell to read a file or all the files in a\ndirectory, and turn them into (path, bytestring) pairs\nembedded in your Haskell code.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/file-embed-0.0.13.0.tar.gz";
      sha256 = "d13068abb0bd22c5d118164734a097dc591977b2c7561d912af9097803c6e1ea";
      });
    }) // {
    package-description-override = "name:            file-embed\nversion:         0.0.13.0\nlicense:         BSD3\nlicense-file:    LICENSE\nauthor:          Michael Snoyman <michael@snoyman.com>\nmaintainer:      Michael Snoyman <michael@snoyman.com>\nsynopsis:        Use Template Haskell to embed file contents directly.\ndescription:     Use Template Haskell to read a file or all the files in a\n                 directory, and turn them into (path, bytestring) pairs\n                 embedded in your Haskell code.\ncategory:        Data\nstability:       Stable\ncabal-version:   >= 1.10\nbuild-type:      Simple\nhomepage:        https://github.com/snoyberg/file-embed\nextra-source-files: test/main.hs, test/sample/foo, test/sample/bar/baz,\n                    ChangeLog.md\n                    README.md\n\nlibrary\n    default-language: Haskell2010\n    build-depends:   base               >= 4.9.1   && < 5\n                   , bytestring         >= 0.9.1.4\n                   , directory          >= 1.0.0.3\n                   , template-haskell\n                   , filepath\n    exposed-modules: Data.FileEmbed\n    ghc-options:     -Wall\n\ntest-suite test\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: main.hs\n    hs-source-dirs: test\n    build-depends: base\n                 , file-embed\n                 , filepath\n\nsource-repository head\n  type:     git\n  location: https://github.com/snoyberg/file-embed\n";
    }