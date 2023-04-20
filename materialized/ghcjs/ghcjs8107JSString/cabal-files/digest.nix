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
    flags = { bytestring-in-base = false; };
    package = {
      specVersion = "1.6";
      identifier = { name = "digest"; version = "0.0.1.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2009 Eugene Kirpichov";
      maintainer = "Eugene Kirpichov <ekirpichov@gmail.com>";
      author = "Eugene Kirpichov <ekirpichov@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "Various cryptographic hashes for bytestrings; CRC32 and Adler32 for now.";
      description = "This package provides efficient cryptographic hash implementations for\nstrict and lazy bytestrings. For now, CRC32 and Adler32 are supported;\nthey are implemented as FFI bindings to efficient code from zlib.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if flags.bytestring-in-base
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (!(!system.isWindows)) (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"));
        libs = (pkgs.lib).optional (!system.isWindows) (pkgs."z" or (errorHandler.sysDepError "z"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/digest-0.0.1.2.tar.gz";
      sha256 = "641717eb16392abf8965986a9e8dc21eebf1d97775bbb6923c7b7f8fee17fe11";
      });
    }) // {
    package-description-override = "name:            digest\nversion:         0.0.1.2\ncopyright:       (c) 2009 Eugene Kirpichov\nlicense:         BSD3\nlicense-file:    LICENSE\nauthor:          Eugene Kirpichov <ekirpichov@gmail.com>\nmaintainer:      Eugene Kirpichov <ekirpichov@gmail.com>\ncategory:        Cryptography\nsynopsis:        Various cryptographic hashes for bytestrings; CRC32 and Adler32 for now.\ndescription:     This package provides efficient cryptographic hash implementations for  \n                 strict and lazy bytestrings. For now, CRC32 and Adler32 are supported; \n                 they are implemented as FFI bindings to efficient code from zlib.\nstability:       provisional\nbuild-type:      Simple\ncabal-version:   >= 1.6\n\nextra-source-files:\n  testing/trivial-reference.c\n  testing/trivial.expected\n  testing/trivial.hs\n\nflag bytestring-in-base\n  description: In the ghc-6.6 era the bytestring modules were\n               included in the base package.\n  default: False\n\nsource-repository head\n  type: git\n  location: git://github.com/jkff/digest\n\nlibrary\n  exposed-modules: Data.Digest.CRC32,\n                   Data.Digest.Adler32\n  extensions:      CPP, ForeignFunctionInterface\n  build-depends: base < 5\n  if flag(bytestring-in-base)\n    -- bytestring was in base-2.0 and 2.1.1\n    build-depends: base >= 2.0 && < 2.2\n    cpp-options: -DBYTESTRING_IN_BASE\n  else\n    build-depends: base < 2.0 || >= 2.2, bytestring >= 0.9\n  includes:        zlib.h\n  ghc-options:     -Wall\n  if !os(windows)\n    extra-libraries: z\n  else\n    build-depends: zlib\n";
    }