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
      non-blocking-ffi = false;
      pkg-config = false;
      bundled-c-zlib = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "zlib"; version = "0.6.3.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2006-2016 Duncan Coutts";
      maintainer = "Duncan Coutts <duncan@community.haskell.org>, Andrew Lelechenko <andrew.lelechenko@gmail.com>, Emily Pillmore <emilypi@cohomolo.gy>, Herbert Valerio Riedel <hvr@gnu.org>";
      author = "Duncan Coutts <duncan@community.haskell.org>";
      homepage = "";
      url = "";
      synopsis = "Compression and decompression in the gzip and zlib formats";
      description = "This package provides a pure interface for compressing and\ndecompressing streams of data represented as lazy\n'ByteString's. It uses the\n<https://en.wikipedia.org/wiki/Zlib zlib C library>\nso it has high performance. It supports the \\\"zlib\\\",\n\\\"gzip\\\" and \\\"raw\\\" compression formats.\n\nIt provides a convenient high level API suitable for most\ntasks and for the few cases where more control is needed it\nprovides access to the full zlib feature set.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.0" && (compiler.version).lt "8.0.3")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        libs = (pkgs.lib).optionals (!(flags.pkg-config && !(compiler.isGhcjs && true) && !system.isGhcjs)) ((pkgs.lib).optional (!system.isWindows && !flags.bundled-c-zlib && !(compiler.isGhcjs && true) && !system.isGhcjs) (pkgs."z" or (errorHandler.sysDepError "z")));
        pkgconfig = (pkgs.lib).optional (flags.pkg-config && !(compiler.isGhcjs && true) && !system.isGhcjs) (pkgconfPkgs."zlib" or (errorHandler.pkgConfDepError "zlib"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/zlib-0.6.3.0.tar.gz";
      sha256 = "9eaa989ad4534438b5beb51c1d3a4c8f6a088fdff0b259a5394fbf39aaee04da";
      });
    }) // {
    package-description-override = "cabal-version:   >= 1.10\nname:            zlib\nversion:         0.6.3.0\n\ncopyright:       (c) 2006-2016 Duncan Coutts\nlicense:         BSD3\nlicense-file:    LICENSE\nauthor:          Duncan Coutts <duncan@community.haskell.org>\nmaintainer:      Duncan Coutts <duncan@community.haskell.org>, Andrew Lelechenko <andrew.lelechenko@gmail.com>, Emily Pillmore <emilypi@cohomolo.gy>, Herbert Valerio Riedel <hvr@gnu.org>\nbug-reports:     https://github.com/haskell/zlib/issues\ncategory:        Codec\nsynopsis:        Compression and decompression in the gzip and zlib formats\ndescription:     This package provides a pure interface for compressing and\n                 decompressing streams of data represented as lazy\n                 'ByteString's. It uses the\n                 <https://en.wikipedia.org/wiki/Zlib zlib C library>\n                 so it has high performance. It supports the \\\"zlib\\\",\n                 \\\"gzip\\\" and \\\"raw\\\" compression formats.\n                 .\n                 It provides a convenient high level API suitable for most\n                 tasks and for the few cases where more control is needed it\n                 provides access to the full zlib feature set.\nbuild-type:      Simple\n\ntested-with:     GHC == 7.0.4\n               , GHC == 7.2.2\n               , GHC == 7.4.2\n               , GHC == 7.6.3\n               , GHC == 7.8.4\n               , GHC == 7.10.3\n               , GHC == 8.0.2\n               , GHC == 8.2.2\n               , GHC == 8.4.4\n               , GHC == 8.6.5\n               , GHC == 8.8.4\n               , GHC == 8.10.7\n               , GHC == 9.0.2\n               , GHC == 9.2.2\n\nextra-source-files: changelog\n                    README.md\n                    -- zlib C sources (for Windows)\n                    cbits/crc32.h cbits/inffast.h cbits/inflate.h\n                    cbits/trees.h cbits/deflate.h cbits/inffixed.h\n                    cbits/inftrees.h cbits/zutil.h cbits/gzguts.h\n                    -- test data files\n                    test/data/bad-crc.gz test/data/custom-dict.zlib\n                    test/data/custom-dict.zlib-dict test/data/hello.gz\n                    test/data/not-gzip test/data/two-files.gz\n                    -- demo programs:\n                    examples/gzip.hs examples/gunzip.hs\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell/zlib.git\n\nflag non-blocking-ffi\n  default:     False\n  manual:      True\n  description: The (de)compression calls can sometimes take a long time, which\n               prevents other Haskell threads running. Enabling this flag\n               avoids this unfairness, but with greater overall cost.\n\nflag pkg-config\n  default:     False\n  manual:      True\n  description: Use @pkg-config(1)@ to locate foreign @zlib@ library.\n\nflag bundled-c-zlib\n  default:     False\n  manual:      True\n  description: Use the bundled zlib C sources. Requires pkg-config to be False.\n               For windows, this is the default.\n\n\nlibrary\n  exposed-modules: Codec.Compression.GZip,\n                   Codec.Compression.Zlib,\n                   Codec.Compression.Zlib.Raw,\n                   Codec.Compression.Zlib.Internal\n  other-modules:   Codec.Compression.Zlib.Stream,\n                   Codec.Compression.Zlib.ByteStringCompat\n\n  if impl(ghc < 7)\n    default-language: Haskell98\n    default-extensions: PatternGuards\n  else\n    default-language: Haskell2010\n\n  other-extensions: CPP, ForeignFunctionInterface, RankNTypes, BangPatterns,\n                    DeriveDataTypeable\n  if impl(ghc >= 7.2)\n    other-extensions: DeriveGeneric\n  if impl(ghc >= 7.6)\n    other-extensions: CApiFFI\n\n  build-depends:   base >= 4 && < 4.18,\n                   bytestring >= 0.9 && < 0.12\n  if impl(ghc >= 7.0 && < 8.0.3)\n    build-depends: ghc-prim\n\n  includes:        zlib.h\n  ghc-options:     -Wall -fwarn-tabs\n  if flag(non-blocking-ffi)\n    cpp-options:   -DNON_BLOCKING_FFI\n  if flag(pkg-config) && !impl(ghcjs) && !os(ghcjs)\n    -- NB: pkg-config is available on windows as well when using msys2\n    pkgconfig-depends: zlib\n  else\n    -- don't use pkg-config\n    if !os(windows) && !flag(bundled-c-zlib) && !impl(ghcjs) && !os(ghcjs)\n      -- Normally we use the the standard system zlib.\n      extra-libraries: z\n    else\n      -- However for the benefit of users of Windows (which does not have zlib\n      -- by default) we bundle a complete copy of the C sources of zlib-1.2.11\n      c-sources:   cbits/adler32.c cbits/compress.c cbits/crc32.c\n                   cbits/deflate.c cbits/infback.c\n                   cbits/inffast.c cbits/inflate.c cbits/inftrees.c\n                   cbits/trees.c cbits/uncompr.c cbits/zutil.c\n      include-dirs:  cbits\n      install-includes: zlib.h zconf.h\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is:         Test.hs\n  other-modules:   Utils,\n                   Test.Codec.Compression.Zlib.Internal,\n                   Test.Codec.Compression.Zlib.Stream\n  hs-source-dirs:  test\n  default-language: Haskell2010\n  build-depends:   base, bytestring, zlib,\n                   QuickCheck       == 2.*,\n                   tasty            >= 0.8 && < 1.5,\n                   tasty-quickcheck >= 0.8 && < 0.11\n  ghc-options:     -Wall\n";
    }