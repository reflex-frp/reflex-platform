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
      identifier = { name = "cryptohash"; version = "0.11.9"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-cryptohash";
      url = "";
      synopsis = "collection of crypto hashes, fast, pure and practical";
      description = "DEPRECATED: this library is still fully functional, but please use cryptonite for new projects\nand convert old one to use cryptonite. This is where things are at nowadays.\n\nA collection of crypto hashes, with a practical incremental and one-pass, pure APIs,\nwith performance close to the fastest implementations available in other languages.\n\nThe implementations are made in C with a haskell FFI wrapper that hide the C implementation.\n\nSimple examples using the unified API:\n\n> import Crypto.Hash\n>\n> sha1 :: ByteString -> Digest SHA1\n> sha1 = hash\n>\n> hexSha3_512 :: ByteString -> String\n> hexSha3_512 bs = show (hash bs :: Digest SHA3_512)\n\nSimple examples using the module API:\n\n> import qualified Crypto.Hash.SHA1 as SHA1\n>\n> main = putStrLn $ show $ SHA1.hash (Data.ByteString.pack [0..255])\n\n> import qualified Crypto.Hash.SHA3 as SHA3\n>\n> main = putStrLn $ show $ digest\n>   where digest = SHA3.finalize ctx\n>         ctx    = foldl' SHA3.update iCtx (map Data.ByteString.pack [ [1,2,3], [4,5,6] ]\n>         iCtx   = SHA3.init 224";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        buildable = true;
        };
      tests = {
        "test-kat" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-hashes" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            ];
          buildable = true;
          };
        "bench-hmac" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
            ];
          buildable = true;
          };
        "bench-api" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cryptohash-0.11.9.tar.gz";
      sha256 = "c28f847fc1fcd65b6eea2e74a100300af940919f04bb21d391f6a773968f22fb";
      });
    }) // {
    package-description-override = "Name:                cryptohash\nVersion:             0.11.9\nDescription:\n    DEPRECATED: this library is still fully functional, but please use cryptonite for new projects\n    and convert old one to use cryptonite. This is where things are at nowadays.\n    .\n    A collection of crypto hashes, with a practical incremental and one-pass, pure APIs,\n    with performance close to the fastest implementations available in other languages.\n    .\n    The implementations are made in C with a haskell FFI wrapper that hide the C implementation.\n    .\n    Simple examples using the unified API:\n    .\n    > import Crypto.Hash\n    >\n    > sha1 :: ByteString -> Digest SHA1\n    > sha1 = hash\n    >\n    > hexSha3_512 :: ByteString -> String\n    > hexSha3_512 bs = show (hash bs :: Digest SHA3_512)\n    .\n    Simple examples using the module API:\n    .\n    > import qualified Crypto.Hash.SHA1 as SHA1\n    >\n    > main = putStrLn $ show $ SHA1.hash (Data.ByteString.pack [0..255])\n    .\n    > import qualified Crypto.Hash.SHA3 as SHA3\n    >\n    > main = putStrLn $ show $ digest\n    >   where digest = SHA3.finalize ctx\n    >         ctx    = foldl' SHA3.update iCtx (map Data.ByteString.pack [ [1,2,3], [4,5,6] ]\n    >         iCtx   = SHA3.init 224\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\nSynopsis:            collection of crypto hashes, fast, pure and practical\nCategory:            Data, Cryptography\nBuild-Type:          Simple\nCabal-Version:       >=1.8\nHomepage:            http://github.com/vincenthz/hs-cryptohash\n\nextra-source-files:\n  cbits/bitfn.h cbits/sha512.h cbits/sha3.h\n  cbits/skein.h cbits/skein256.h cbits/skein512.h\n  README.md\n\nLibrary\n  Build-Depends:     base >= 4 && < 6, bytestring, byteable, cryptonite >= 0.13, memory, ghc-prim\n  if impl(ghc >= 7.2.1)\n    Extensions:      Trustworthy\n  Extensions:        ForeignFunctionInterface\n  Exposed-modules:   Crypto.Hash\n                     Crypto.Hash.Types\n                     Crypto.MAC\n                     Crypto.Hash.SHA1\n                     Crypto.Hash.SHA224\n                     Crypto.Hash.SHA256\n                     Crypto.Hash.SHA384\n                     Crypto.Hash.SHA512\n                     Crypto.Hash.SHA512t\n                     Crypto.Hash.SHA3\n                     Crypto.Hash.MD2\n                     Crypto.Hash.MD4\n                     Crypto.Hash.MD5\n                     Crypto.Hash.RIPEMD160\n                     Crypto.Hash.Skein256\n                     Crypto.Hash.Skein512\n                     Crypto.Hash.Tiger\n                     Crypto.Hash.Whirlpool\n                     Crypto.MAC.HMAC\n                     Crypto.MAC.SHA3\n  Other-modules:     Crypto.Hash.Internal\n  ghc-options:       -Wall -optc-O3 -fno-cse -fwarn-tabs\n  C-sources:         cbits/sha512.c\n                     cbits/sha3.c\n                     cbits/skein256.c\n                     cbits/skein512.c\n  Include-Dirs:      cbits\n  if (arch(i386) || arch(x86_64))\n    cpp-options: -DARCH_X86\n\nTest-Suite test-kat\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    Tests\n  Main-Is:           KAT.hs\n  Build-depends:     base >= 4 && < 5\n                   , bytestring\n                   , byteable\n                   , HUnit\n                   , QuickCheck >= 2\n                   , tasty\n                   , tasty-quickcheck\n                   , tasty-hunit\n                   , cryptohash\n\nBenchmark bench-hashes\n  Main-Is:           Bench.hs\n  hs-source-dirs:    Bench\n  type:              exitcode-stdio-1.0\n  Build-depends:     base >= 4, bytestring, criterion, cryptohash\n\nBenchmark bench-hmac\n  Main-Is:           BenchHMAC.hs\n  hs-source-dirs:    Bench\n  type:              exitcode-stdio-1.0\n  Build-depends:     base >= 4, bytestring, criterion, cryptohash, byteable\n\nBenchmark bench-api\n  Main-Is:           BenchAPI.hs\n  hs-source-dirs:    Bench\n  type:              exitcode-stdio-1.0\n  Build-depends:     base >= 4, bytestring, criterion, cryptohash, byteable\n\nsource-repository head\n  type:     git\n  location: git://github.com/vincenthz/hs-cryptohash\n";
    }