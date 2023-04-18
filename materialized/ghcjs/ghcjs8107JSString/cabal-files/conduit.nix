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
      identifier = { name = "conduit"; version = "1.3.4.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/snoyberg/conduit";
      url = "";
      synopsis = "Streaming data processing library.";
      description = "`conduit` is a solution to the streaming data problem, allowing for production,\ntransformation, and consumption of streams of data in constant memory. It is an\nalternative to lazy I\\/O which guarantees deterministic resource handling.\n\nFor more information about conduit in general, and how this package in\nparticular fits into the ecosystem, see [the conduit\nhomepage](https://github.com/snoyberg/conduit#readme).\n\nHackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/conduit>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "conduit-test" = {
          depends = [
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "optimize-201408" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."kan-extensions" or (errorHandler.buildDepError "kan-extensions"))
            ];
          buildable = true;
          };
        "unfused" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/conduit-1.3.4.1.tar.gz";
      sha256 = "85743b8d5f2d5779ccb7459b5a919c5786707af23fe7a065d281ee8e6dc226f1";
      });
    }) // {
    package-description-override = "Name:                conduit\nVersion:             1.3.4.1\nSynopsis:            Streaming data processing library.\ndescription:\n    `conduit` is a solution to the streaming data problem, allowing for production,\n    transformation, and consumption of streams of data in constant memory. It is an\n    alternative to lazy I\\/O which guarantees deterministic resource handling.\n    .\n    For more information about conduit in general, and how this package in\n    particular fits into the ecosystem, see [the conduit\n    homepage](https://github.com/snoyberg/conduit#readme).\n    .\n    Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/conduit>.\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman\nMaintainer:          michael@snoyman.com\nCategory:            Data, Conduit\nBuild-type:          Simple\nCabal-version:       >=1.10\nHomepage:            http://github.com/snoyberg/conduit\nextra-source-files:  test/main.hs\n                   , test/doctests.hs\n                   , test/subdir/dummyfile.txt\n                   , README.md\n                   , ChangeLog.md\n                   , fusion-macros.h\n\nLibrary\n  default-language:    Haskell2010\n  hs-source-dirs:      src\n  Exposed-modules:     Data.Conduit\n                       Data.Conduit.Combinators\n                       Data.Conduit.List\n                       Data.Conduit.Internal\n                       Data.Conduit.Lift\n                       Data.Conduit.Internal.Fusion\n                       Data.Conduit.Internal.List.Stream\n                       Data.Conduit.Combinators.Stream\n                       Conduit\n  other-modules:       Data.Conduit.Internal.Pipe\n                       Data.Conduit.Internal.Conduit\n                       Data.Conduit.Combinators.Unqualified\n                       Data.Streaming.FileRead\n                       Data.Streaming.Filesystem\n  Build-depends:       base                     >= 4.9          && < 5\n                     , resourcet                >= 1.2          && < 1.3\n                     , transformers             >= 0.4\n                     , mtl\n                     , primitive\n                     , unliftio-core\n                     , exceptions\n                     , mono-traversable         >= 1.0.7\n                     , vector\n                     , bytestring\n                     , text\n                     , filepath\n                     , directory\n\n  if os(windows)\n    build-depends:     Win32\n    other-modules:     System.Win32File\n    cpp-options:       -DWINDOWS\n  else\n    build-depends:     unix\n\n  ghc-options:         -Wall\n  include-dirs:        .\n\ntest-suite conduit-test\n    default-language:    Haskell2010\n    hs-source-dirs: test\n    main-is: main.hs\n    other-modules: Data.Conduit.Extra.ZipConduitSpec\n                 , Data.Conduit.StreamSpec\n                 , Spec\n                 , StreamSpec\n    type: exitcode-stdio-1.0\n    cpp-options:   -DTEST\n    build-depends:   conduit\n                   , base\n                   , hspec >= 1.3\n                   , QuickCheck >= 2.7\n                   , transformers\n                   , mtl\n                   , resourcet\n                   , containers\n                   , exceptions >= 0.6\n                   , safe\n                   , split >= 0.2.0.0\n                   , mono-traversable\n                   , text\n                   , vector\n                   , directory\n                   , bytestring\n                   , silently\n                   , filepath\n                   , unliftio >= 0.2.4.0\n    ghc-options:     -Wall\n\n  if os(windows)\n    cpp-options:     -DWINDOWS\n\n--test-suite doctests\n--    hs-source-dirs: test\n--    main-is: doctests.hs\n--    type: exitcode-stdio-1.0\n--    ghc-options: -threaded\n--    build-depends: base, directory, doctest >= 0.8\n\n-- benchmark utf8-memory-usage\n--     type: exitcode-stdio-1.0\n--     hs-source-dirs: benchmarks\n--     build-depends:  base\n--                   , text-stream-decode\n--                   , bytestring\n--                   , text\n--                   , conduit\n--     main-is:        utf8-memory-usage.hs\n--     ghc-options:    -Wall -O2 -with-rtsopts=-s\n\nbenchmark optimize-201408\n    default-language:    Haskell2010\n    type: exitcode-stdio-1.0\n    hs-source-dirs: benchmarks\n    build-depends:  base\n                  , conduit\n                  , vector\n                  , deepseq\n                  , containers\n                  , transformers\n                  , hspec\n                  , mwc-random\n                  , gauge\n                  , kan-extensions\n    main-is:        optimize-201408.hs\n    ghc-options:    -Wall -O2 -rtsopts\n\nbenchmark unfused\n    default-language:    Haskell2010\n    type: exitcode-stdio-1.0\n    hs-source-dirs: benchmarks\n    build-depends:  base\n                  , conduit\n                  , gauge\n                  , transformers\n    main-is:        unfused.hs\n    ghc-options:    -Wall -O2 -rtsopts\n\nsource-repository head\n  type:     git\n  location: git://github.com/snoyberg/conduit.git\n";
    }