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
    flags = { executable = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "zip-archive"; version = "0.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "jgm@berkeley.edu";
      author = "John MacFarlane";
      homepage = "http://github.com/jgm/zip-archive";
      url = "";
      synopsis = "Library for creating and modifying zip archives.";
      description = "The zip-archive library provides functions for creating, modifying, and\nextracting files from zip archives. The zip archive format is\ndocumented in <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>.\n\nCertain simplifying assumptions are made about the zip archives: in\nparticular, there is no support for strong encryption, zip files that\nspan multiple disks, ZIP64, OS-specific file attributes, or compression\nmethods other than Deflate. However, the library should be able to read\nthe most common zip archives, and the archives it produces should be\nreadable by all standard unzip programs.\n\nArchives are built and extracted in memory, so manipulating large zip\nfiles will consume a lot of memory. If you work with large zip files or\nneed features not supported by this library, a better choice may be\n<http://hackage.haskell.org/package/zip zip>, which uses a\nmemory-efficient streaming approach. However, zip can only read and\nwrite archives inside instances of MonadIO, so zip-archive is a better\nchoice if you want to manipulate zip archives in \"pure\" contexts.\n\nAs an example of the use of the library, a standalone zip archiver and\nextracter is provided in the source distribution.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      exes = {
        "zip-archive" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."zip-archive" or (errorHandler.buildDepError "zip-archive"))
            ];
          buildable = if flags.executable then true else false;
          };
        };
      tests = {
        "test-zip-archive" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."zip-archive" or (errorHandler.buildDepError "zip-archive"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          build-tools = [
            (hsPkgs.buildPackages.unzip.components.exes.unzip or (pkgs.buildPackages.unzip or (errorHandler.buildToolDepError "unzip:unzip")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/zip-archive-0.4.1.tar.gz";
      sha256 = "c5d5c9976241dcc25b0d8753dc526bb1bfef60f30dee38c53a7ae56e6be9b1b1";
      });
    }) // {
    package-description-override = "Name:                zip-archive\r\nVersion:             0.4.1\r\nx-revision: 1\r\nCabal-Version:       2.0\r\nBuild-type:          Simple\r\nSynopsis:            Library for creating and modifying zip archives.\r\nDescription:\r\n   The zip-archive library provides functions for creating, modifying, and\r\n   extracting files from zip archives. The zip archive format is\r\n   documented in <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>.\r\n   .\r\n   Certain simplifying assumptions are made about the zip archives: in\r\n   particular, there is no support for strong encryption, zip files that\r\n   span multiple disks, ZIP64, OS-specific file attributes, or compression\r\n   methods other than Deflate. However, the library should be able to read\r\n   the most common zip archives, and the archives it produces should be\r\n   readable by all standard unzip programs.\r\n   .\r\n   Archives are built and extracted in memory, so manipulating large zip\r\n   files will consume a lot of memory. If you work with large zip files or\r\n   need features not supported by this library, a better choice may be\r\n   <http://hackage.haskell.org/package/zip zip>, which uses a\r\n   memory-efficient streaming approach. However, zip can only read and\r\n   write archives inside instances of MonadIO, so zip-archive is a better\r\n   choice if you want to manipulate zip archives in \"pure\" contexts.\r\n   .\r\n   As an example of the use of the library, a standalone zip archiver and\r\n   extracter is provided in the source distribution.\r\nCategory:            Codec\r\nTested-with:         GHC == 7.8.2, GHC == 7.10.3, GHC == 8.0.2,\r\n                     GHC == 8.2.2, GHC == 8.4.3, GHC == 8.6.1\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nHomepage:            http://github.com/jgm/zip-archive\r\nAuthor:              John MacFarlane\r\nMaintainer:          jgm@berkeley.edu\r\nExtra-Source-Files:  changelog\r\n                     README.markdown\r\n                     tests/test4.zip\r\n                     tests/test4/a.txt\r\n                     tests/test4/b.bin\r\n                     \"tests/test4/c/with spaces.txt\"\r\n                     tests/zip_with_symlinks.zip\r\n                     tests/zip_with_password.zip\r\n                     tests/zip_with_evil_path.zip\r\n\r\nSource-repository    head\r\n  type:              git\r\n  location:          git://github.com/jgm/zip-archive.git\r\n\r\nflag executable\r\n  Description:       Build the Zip executable.\r\n  Default:           False\r\n\r\nLibrary\r\n  Build-depends:     base >= 4.5 && < 5,\r\n                     pretty,\r\n                     containers,\r\n                     binary >= 0.6,\r\n                     zlib,\r\n                     filepath,\r\n                     bytestring >= 0.10.0,\r\n                     array,\r\n                     mtl,\r\n                     text >= 0.11,\r\n                     digest >= 0.0.0.1,\r\n                     directory >= 1.2.0,\r\n                     time\r\n  Exposed-modules:   Codec.Archive.Zip\r\n  Default-Language:  Haskell98\r\n  Hs-Source-Dirs:    src\r\n  Ghc-Options:       -Wall\r\n  if os(windows)\r\n    cpp-options:     -D_WINDOWS\r\n  else\r\n    Build-depends:   unix\r\n\r\nExecutable zip-archive\r\n  if flag(executable)\r\n    Buildable:       True\r\n  else\r\n    Buildable:       False\r\n  Main-is:           Main.hs\r\n  Hs-Source-Dirs:    .\r\n  Build-Depends:     base >= 4.2 && < 5,\r\n                     directory >= 1.1,\r\n                     bytestring >= 0.9.0,\r\n                     zip-archive\r\n  Other-Modules:     Paths_zip_archive\r\n  Autogen-Modules:   Paths_zip_archive\r\n  Ghc-Options:       -Wall\r\n  Default-Language:  Haskell98\r\n\r\nTest-Suite test-zip-archive\r\n  Type:           exitcode-stdio-1.0\r\n  Main-Is:        test-zip-archive.hs\r\n  Hs-Source-Dirs: tests\r\n  Build-Depends:  base >= 4.2 && < 5,\r\n                  directory >= 1.3, bytestring >= 0.9.0, process, time,\r\n                  HUnit, zip-archive, temporary, filepath\r\n  Default-Language:  Haskell98\r\n  Ghc-Options:    -Wall\r\n  if os(windows)\r\n    cpp-options:     -D_WINDOWS\r\n  else\r\n    Build-depends:   unix\r\n  build-tools: unzip\r\n";
    }