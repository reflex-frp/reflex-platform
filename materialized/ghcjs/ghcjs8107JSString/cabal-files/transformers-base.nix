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
    flags = { orphaninstances = true; };
    package = {
      specVersion = "1.8";
      identifier = { name = "transformers-base"; version = "0.4.5.2"; };
      license = "BSD-3-Clause";
      copyright = "2011 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>,\nBas van Dijk <v.dijk.bas@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>,\nBas van Dijk <v.dijk.bas@gmail.com>";
      homepage = "https://github.com/mvv/transformers-base";
      url = "";
      synopsis = "Lift computations from the bottom of a transformer stack";
      description = "This package provides a straightforward port of @monadLib@'s BaseM\ntypeclass to @transformers@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (flags.orphaninstances) (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/transformers-base-0.4.5.2.tar.gz";
      sha256 = "d0c80c63fdce6a077dd8eda4f1ff289b85578703a3f1272e141d400fe23245e8";
      });
    }) // {
    package-description-override = "Name: transformers-base\nVersion: 0.4.5.2\nCategory: Control\nStability: experimental\nSynopsis: Lift computations from the bottom of a transformer stack\nDescription:\n  This package provides a straightforward port of @monadLib@'s BaseM\n  typeclass to @transformers@.\n\nHomepage: https://github.com/mvv/transformers-base\nBug-Reports: https://github.com/mvv/transformers-base/issues\n\nAuthor:\n  Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>,\n  Bas van Dijk <v.dijk.bas@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright:\n  2011 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>,\n       Bas van Dijk <v.dijk.bas@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nTested-With: GHC==7.0.4, GHC==7.4.2, GHC==7.6.3, GHC==7.8.4,\n             GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.1\n\nCabal-Version: >= 1.8\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/transformers-base.git\n\nFlag OrphanInstances\n  Description:\n    Import orphan Applicative instances for lazy and strict ST if needed\n  Default: True\n\nLibrary\n  Build-Depends:\n    base                >= 3 && < 5 && (< 4.4 || >= 4.5),\n    stm                 >= 2.3,\n    transformers        >= 0.2,\n    transformers-compat >= 0.6.1\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  if flag(OrphanInstances)\n    Build-Depends:\n      base-orphans >= 0.3\n    CPP-Options: -DHS_TRANSFORMERS_BASE__ORPHANS=1\n  else\n    CPP-Options: -DHS_TRANSFORMERS_BASE__ORPHANS=0\n  Exposed-Modules:\n    Control.Monad.Base\n";
    }