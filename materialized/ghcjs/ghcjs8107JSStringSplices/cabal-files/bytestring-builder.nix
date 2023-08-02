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
    flags = { bytestring_has_builder = true; };
    package = {
      specVersion = "1.8";
      identifier = { name = "bytestring-builder"; version = "0.10.8.2.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2010 Jasper Van der Jeugt\n(c) 2010-2013 Simon Meier\n(c) 2012-2013 Duncan Coutts";
      maintainer = "Leon P Smith <leon@melding-monads.com>";
      author = "Simon Meier, Jasper Van der Jeugt, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "The new bytestring builder, packaged outside of GHC";
      description = "This is the bytestring builder that is debuting in bytestring-0.10.4.0, which\nshould be shipping with GHC 7.8, probably late in 2013.  This builder has\nseveral nice simplifications and improvements, and more out-of-box\nfunctionality than the older blaze-builder.\n\nNote that this package detects which version of bytestring you are compiling\nagainst,  and if you are compiling against bytestring-0.10.4 or later, will\nbe an empty package.\n\nThis package lets the new interface and implementation be used with most\nolder compilers without upgrading bytestring, which can be rather\nproblematic.  In conjunction with blaze-builder-0.4 or later,  which\noffers an implementation of blaze-builder in terms of bytestring-builder,\nthis should let most people try the new interface and implementation without\ncausing undue compatibility problems with packages that depend on\nblaze-builder.\n\nGHC 7.6 did debut an almost identical interface and implementation, but with\nslightly different module names and organization.   Trying to re-export/rename\nthe builder provided with 7.6 did not turn out to be very practical,  because\nthis interface includes new functions that rely on Builder internals,\nwhich are not exported in 7.6.  Furthermore, these module names should be\ndeprecated in 7.10.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ [
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bytestring-builder-0.10.8.2.0.tar.gz";
      sha256 = "27faef6db27c5be5a3715fd68b93725853e0e668849eaf92ce7c33cef9cb2c3f";
      });
    }) // {
    package-description-override = "name:                bytestring-builder\nversion:             0.10.8.2.0\nsynopsis:            The new bytestring builder, packaged outside of GHC\ndescription:\n  This is the bytestring builder that is debuting in bytestring-0.10.4.0, which\n  should be shipping with GHC 7.8, probably late in 2013.  This builder has\n  several nice simplifications and improvements, and more out-of-box\n  functionality than the older blaze-builder.\n  .\n  Note that this package detects which version of bytestring you are compiling\n  against,  and if you are compiling against bytestring-0.10.4 or later, will\n  be an empty package.\n  .\n  This package lets the new interface and implementation be used with most\n  older compilers without upgrading bytestring, which can be rather\n  problematic.  In conjunction with blaze-builder-0.4 or later,  which\n  offers an implementation of blaze-builder in terms of bytestring-builder,\n  this should let most people try the new interface and implementation without\n  causing undue compatibility problems with packages that depend on\n  blaze-builder.\n  .\n  GHC 7.6 did debut an almost identical interface and implementation, but with\n  slightly different module names and organization.   Trying to re-export/rename\n  the builder provided with 7.6 did not turn out to be very practical,  because\n  this interface includes new functions that rely on Builder internals,\n  which are not exported in 7.6.  Furthermore, these module names should be\n  deprecated in 7.10.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Simon Meier, Jasper Van der Jeugt, Duncan Coutts\nmaintainer:          Leon P Smith <leon@melding-monads.com>\ncopyright:           (c) 2010 Jasper Van der Jeugt\n                     (c) 2010-2013 Simon Meier\n                     (c) 2012-2013 Duncan Coutts\ncategory:            Data\nbuild-type:          Simple\nextra-source-files:\n                     cbits/*.c\n\n                     src/Data/ByteString/*.hs\n                     src/Data/ByteString/Builder/*.hs\n                     src/Data/ByteString/Builder/Prim/*.hs\n                     src/Data/ByteString/Builder/Prim/Internal/*.hs\n                     src/Data/ByteString/Short/*.hs\n\n                     CHANGELOG.md\ncabal-version:       >=1.8\n\nsource-repository head\n  type:     git\n  location: http://github.com/lpsmith/bytestring-builder\n\nsource-repository this\n  type:     git\n  location: http://github.com/lpsmith/bytestring-builder\n  tag:      v0.10.8.1.0\n\nFlag bytestring_has_builder\n  default: True\n\nlibrary\n  build-depends:     base >= 4.2 && < 5,\n                     deepseq\n\n  if flag(bytestring_has_builder)\n      build-depends:     bytestring >= 0.10.4\n      -- package is empty for bytestring >= 0.10.4\n  else\n      build-depends:     bytestring >= 0.9     && < 0.10.2\n      hs-source-dirs: src\n      c-sources: cbits/itoa.c\n                 cbits/fpstring.c\n      exposed-modules:\n                         Data.ByteString.Builder\n                         Data.ByteString.Builder.Extra\n                         Data.ByteString.Builder.Prim\n\n                         -- perhaps only exposed temporarily\n                         Data.ByteString.Builder.Internal\n                         Data.ByteString.Builder.Prim.Internal\n\n                         Data.ByteString.Short\n                         Data.ByteString.Short.Internal\n\n      other-modules:\n                         Data.ByteString.Builder.ASCII\n                         Data.ByteString.Builder.Prim.Binary\n                         Data.ByteString.Builder.Prim.ASCII\n                         Data.ByteString.Builder.Prim.Internal.Floating\n                         Data.ByteString.Builder.Prim.Internal.UncheckedShifts\n                         Data.ByteString.Builder.Prim.Internal.Base16\n";
    }