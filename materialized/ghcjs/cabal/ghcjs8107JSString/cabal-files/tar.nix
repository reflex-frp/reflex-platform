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
    flags = { old-time = false; old-bytestring = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "tar"; version = "0.5.1.1"; };
      license = "BSD-3-Clause";
      copyright = "2007 Bjorn Bringert <bjorn@bringert.net>\n2008-2016 Duncan Coutts <duncan@community.haskell.org>";
      maintainer = "Duncan Coutts <duncan@community.haskell.org>";
      author = "Duncan Coutts <duncan@community.haskell.org>\nBjorn Bringert <bjorn@bringert.net>";
      homepage = "";
      url = "";
      synopsis = "Reading, writing and manipulating \".tar\" archive files.";
      description = "This library is for working with \\\"@.tar@\\\" archive files. It\ncan read and write a range of common variations of archive\nformat including V7, POSIX USTAR and GNU formats.\n\nIt provides support for packing and unpacking portable\narchives. This makes it suitable for distribution but not\nbackup because details like file ownership and exact\npermissions are not preserved.\n\nIt also provides features for random access to archive\ncontent using an index.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (if flags.old-time
          then [
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
            ]
          else [
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ])) ++ (if flags.old-bytestring
          then [
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "properties" = {
          depends = (([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."bytestring-handle" or (errorHandler.buildDepError "bytestring-handle"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ] ++ (if flags.old-time
            then [
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
              ]
            else [
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              ])) ++ (if flags.old-bytestring
            then [
              (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ]
            else [
              (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
              ])) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tar-0.5.1.1.tar.gz";
      sha256 = "b384449f62b2b0aa3e6d2cb1004b8060b01f21ec93e7b63e7af6d8fad8a9f1de";
      });
    }) // {
    package-description-override = "cabal-version:   1.12\r\nname:            tar\r\nversion:         0.5.1.1\r\nx-revision: 5\r\n\r\nlicense:         BSD3\r\nlicense-file:    LICENSE\r\nauthor:          Duncan Coutts <duncan@community.haskell.org>\r\n                 Bjorn Bringert <bjorn@bringert.net>\r\nmaintainer:      Duncan Coutts <duncan@community.haskell.org>\r\nbug-reports:     https://github.com/haskell/tar/issues\r\ncopyright:       2007 Bjorn Bringert <bjorn@bringert.net>\r\n                 2008-2016 Duncan Coutts <duncan@community.haskell.org>\r\ncategory:        Codec\r\nsynopsis:        Reading, writing and manipulating \".tar\" archive files.\r\ndescription:     This library is for working with \\\"@.tar@\\\" archive files. It\r\n                 can read and write a range of common variations of archive\r\n                 format including V7, POSIX USTAR and GNU formats.\r\n                 .\r\n                 It provides support for packing and unpacking portable\r\n                 archives. This makes it suitable for distribution but not\r\n                 backup because details like file ownership and exact\r\n                 permissions are not preserved.\r\n                 .\r\n                 It also provides features for random access to archive\r\n                 content using an index.\r\nbuild-type:      Simple\r\nextra-source-files: changelog.md\r\ntested-with:     GHC==7.0.4, GHC==7.2.2, GHC==7.4.2, GHC==7.6.3,\r\n                 GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.4,\r\n                 GHC==8.6.5, GHC==8.8.3, GHC==8.10.4, GHC==9.0.1\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/haskell/tar.git\r\n\r\nflag old-time\r\n  default: False\r\n\r\nflag old-bytestring\r\n  default: False\r\n\r\nlibrary\r\n\r\n  build-depends: base       >= 4 && < 4.18,\r\n                 filepath             < 1.5,\r\n                 array                < 0.6,\r\n                 containers >= 0.2 && < 0.7,\r\n                 deepseq    >= 1.1 && < 1.5\r\n\r\n  if flag(old-time)\r\n    build-depends: directory < 1.2, old-time < 1.2\r\n  else\r\n    build-depends: directory >= 1.2 && < 1.4, time < 1.13\r\n\r\n  if flag(old-bytestring)\r\n    build-depends: bytestring-builder >= 0.10.4.0.2 && < 0.11, bytestring == 0.9.*\r\n  else\r\n    build-depends: bytestring >= 0.10 && < 0.12\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >= 0.18 && < 0.20\r\n\r\n  exposed-modules:\r\n    Codec.Archive.Tar\r\n    Codec.Archive.Tar.Entry\r\n    Codec.Archive.Tar.Check\r\n    Codec.Archive.Tar.Index\r\n\r\n  other-modules:\r\n    Codec.Archive.Tar.Types\r\n    Codec.Archive.Tar.Read\r\n    Codec.Archive.Tar.Write\r\n    Codec.Archive.Tar.Pack\r\n    Codec.Archive.Tar.Unpack\r\n    Codec.Archive.Tar.Index.StringTable\r\n    Codec.Archive.Tar.Index.IntTrie\r\n\r\n  default-language: Haskell2010\r\n  -- Previously, the package used GHC's default Haskell mode which implies\r\n  -- NDI; so we keep it transitionally enabled here until we've reviewed the\r\n  -- code to make sure there isn't any code relies on NDI and keeps compiling\r\n  -- albeit with different semantics even without NDI\r\n  default-extensions: NondecreasingIndentation\r\n\r\n  other-extensions:\r\n    BangPatterns\r\n    CPP\r\n    DeriveDataTypeable\r\n    GeneralizedNewtypeDeriving\r\n    PatternGuards\r\n    ScopedTypeVariables\r\n\r\n\r\n  ghc-options: -Wall -fno-warn-unused-imports\r\n\r\ntest-suite properties\r\n  type:          exitcode-stdio-1.0\r\n  build-depends: base,\r\n                 filepath,\r\n                 array,\r\n                 containers,\r\n                 deepseq,\r\n                 bytestring-handle,\r\n                 QuickCheck       == 2.*,\r\n                 tasty            >= 0.10 && <1.5,\r\n                 tasty-quickcheck >= 0.8  && <0.11\r\n\r\n  if flag(old-time)\r\n    build-depends: directory < 1.2, old-time\r\n  else\r\n    build-depends: directory >= 1.2, time\r\n\r\n  if flag(old-bytestring)\r\n    build-depends: bytestring-builder, bytestring >= 0.9 && <0.10\r\n  else\r\n    build-depends: bytestring >= 0.10\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >= 0.18 && <0.20\r\n\r\n  default-language: Haskell2010\r\n\r\n  hs-source-dirs: . test\r\n\r\n  main-is: test/Properties.hs\r\n  cpp-options: -DTESTS\r\n\r\n  other-modules:\r\n    Codec.Archive.Tar.Index\r\n    Codec.Archive.Tar.Index.StringTable\r\n    Codec.Archive.Tar.Index.IntTrie\r\n\r\n  -- shared w/ lib:tar component\r\n  other-modules:\r\n    Codec.Archive.Tar\r\n    Codec.Archive.Tar.Check\r\n    Codec.Archive.Tar.Pack\r\n    Codec.Archive.Tar.Read\r\n    Codec.Archive.Tar.Types\r\n    Codec.Archive.Tar.Unpack\r\n    Codec.Archive.Tar.Write\r\n\r\n  other-extensions:\r\n    CPP\r\n    BangPatterns,\r\n    DeriveDataTypeable\r\n    ScopedTypeVariables\r\n\r\n  ghc-options: -fno-ignore-asserts\r\n\r\nbenchmark bench\r\n  type:          exitcode-stdio-1.0\r\n  hs-source-dirs: . bench\r\n  main-is:       bench/Main.hs\r\n  build-depends: base,\r\n                 bytestring >= 0.10,\r\n                 filepath,\r\n                 directory >= 1.2,\r\n                 array,\r\n                 containers,\r\n                 deepseq,\r\n                 time,\r\n                 criterion >= 1.0\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >= 0.18 && <0.20\r\n\r\n  default-language: Haskell2010\r\n\r\n  -- shared w/ lib:tar component\r\n  other-modules:\r\n    Codec.Archive.Tar\r\n    Codec.Archive.Tar.Check\r\n    Codec.Archive.Tar.Index\r\n    Codec.Archive.Tar.Index.IntTrie\r\n    Codec.Archive.Tar.Index.StringTable\r\n    Codec.Archive.Tar.Pack\r\n    Codec.Archive.Tar.Read\r\n    Codec.Archive.Tar.Types\r\n    Codec.Archive.Tar.Unpack\r\n    Codec.Archive.Tar.Write\r\n";
    }