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
    flags = { use-bytestring-builder = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "streaming-commons"; version = "0.2.2.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Emanuel Borsboom";
      homepage = "https://github.com/fpco/streaming-commons";
      url = "";
      synopsis = "Common lower-level functions needed by various streaming data libraries";
      description = "Provides low-dependency functionality commonly needed by various streaming data libraries, such as conduit and pipes.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          ] ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ])) ++ (if flags.use-bytestring-builder
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ]);
        buildable = true;
        };
      tests = {
        "test" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            ] ++ (if flags.use-bytestring-builder
            then [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
              ]
            else [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ])) ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          };
        };
      benchmarks = {
        "count-chars" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            ];
          buildable = true;
          };
        "decode-memory-usage" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            ];
          buildable = true;
          };
        "builder-to-bytestring-io" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            ] ++ (if flags.use-bytestring-builder
            then [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
              ]
            else [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ]);
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/streaming-commons-0.2.2.1.tar.gz";
      sha256 = "306940bf4878a0b714e6746a7f934d018100efc86332c176a648014bfe1e81dd";
      });
    }) // {
    package-description-override = "name:                streaming-commons\nversion:             0.2.2.1\nsynopsis:            Common lower-level functions needed by various streaming data libraries\ndescription:         Provides low-dependency functionality commonly needed by various streaming data libraries, such as conduit and pipes.\nhomepage:            https://github.com/fpco/streaming-commons\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Michael Snoyman, Emanuel Borsboom\nmaintainer:          michael@snoyman.com\n-- copyright:\ncategory:            Data\nbuild-type:          Simple\ncabal-version:       >=1.10\nextra-source-files:\n    test/filesystem/*.txt\n    test/filesystem/bin/*.txt\n    include/*.h\n    cbits/*.c\n    test/LICENSE.gz\n    ChangeLog.md\n    README.md\n\nflag use-bytestring-builder\n  description: Use bytestring-builder package\n  default: False\n\nlibrary\n  default-language: Haskell2010\n  exposed-modules:     Data.Streaming.ByteString.Builder\n                       Data.Streaming.ByteString.Builder.Buffer\n                       Data.Streaming.FileRead\n                       Data.Streaming.Filesystem\n                       Data.Streaming.Network\n                       Data.Streaming.Network.Internal\n                       Data.Streaming.Process\n                       Data.Streaming.Process.Internal\n                       Data.Streaming.Text\n                       Data.Streaming.Zlib\n                       Data.Streaming.Zlib.Lowlevel\n\n  -- Due to cabal bugs, not making inclusion of this dependent on text version.\n  -- For more information, see: https://github.com/fpco/text-stream-decode/issues/1\n  other-modules:       Data.Text.Internal.Unsafe.Char\n                       Data.Text.Internal.Unsafe.Shift\n                       Data.Text.Internal.Encoding.Utf8\n                       Data.Text.Internal.Encoding.Utf16\n                       Data.Text.Internal.Encoding.Utf32\n\n  build-depends:       base >= 4.9 && < 5\n                     , array\n                     , async\n                     , bytestring\n                     , directory\n                     , network >= 2.4.0.0\n                     , random\n                     , process\n                     , stm\n                     , text\n                     , transformers\n                     , zlib\n\n  c-sources:           cbits/zlib-helper.c\n                       cbits/text-helper.c\n  include-dirs:        include\n\n  if os(windows)\n    build-depends:     Win32\n                     , filepath\n    cpp-options:       -DWINDOWS\n    other-modules:     System.Win32File\n  else\n    build-depends:     unix\n\n  if flag(use-bytestring-builder)\n    build-depends:     bytestring < 0.10.2.0\n                     , bytestring-builder\n  else\n    build-depends:     bytestring >= 0.10.2.0\n\ntest-suite test\n    default-language: Haskell2010\n    hs-source-dirs: test\n    main-is:        Spec.hs\n    type:           exitcode-stdio-1.0\n    ghc-options:    -Wall -threaded\n    other-modules:  Data.Streaming.ByteString.BuilderSpec\n                    Data.Streaming.FileReadSpec\n                    Data.Streaming.FilesystemSpec\n                    Data.Streaming.NetworkSpec\n                    Data.Streaming.ProcessSpec\n                    Data.Streaming.TextSpec\n                    Data.Streaming.ZlibSpec\n    build-depends:  base\n                  , streaming-commons\n                  , hspec >= 1.8\n\n                  , QuickCheck\n                  , array\n                  , async\n                  , bytestring\n                  , deepseq\n                  , network >= 2.4.0.0\n                  , text\n                  , zlib\n\n  if flag(use-bytestring-builder)\n    build-depends:     bytestring < 0.10.2.0\n                     , bytestring-builder\n  else\n    build-depends:     bytestring >= 0.10.2.0\n\n  if os(windows)\n    cpp-options:       -DWINDOWS\n  else\n    build-depends:     unix\n\nbenchmark count-chars\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    hs-source-dirs: bench\n    build-depends:  base\n                  , gauge\n                  , bytestring\n                  , text\n                  , streaming-commons\n    main-is:        count-chars.hs\n    ghc-options:    -Wall -O2\n\nbenchmark decode-memory-usage\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    hs-source-dirs: bench\n    build-depends:  base\n                  , bytestring\n                  , text\n                  , streaming-commons\n    main-is:        decode-memory-usage.hs\n    ghc-options:    -Wall -O2 -with-rtsopts=-s\n\nbenchmark builder-to-bytestring-io\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    hs-source-dirs: bench\n    main-is:        builder-to-bytestring-io.hs\n    ghc-options:    -Wall -O2\n    build-depends:  base\n                  , bytestring >= 0.10.2\n                  , gauge\n                  , deepseq\n                  , streaming-commons\n\n  if flag(use-bytestring-builder)\n    build-depends:     bytestring < 0.10.2.0\n                     , bytestring-builder\n  else\n    build-depends:     bytestring >= 0.10.2.0\n\nsource-repository head\n  type:     git\n  location: git://github.com/fpco/streaming-commons.git\n";
    }