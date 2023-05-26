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
      identifier = { name = "syb"; version = "0.7.2.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Sergey Vinokurov <serg.foo@gmail.com>";
      author = "Ralf Lammel, Simon Peyton Jones, Jose Pedro Magalhaes";
      homepage = "http://www.cs.uu.nl/wiki/GenericProgramming/SYB";
      url = "";
      synopsis = "Scrap Your Boilerplate";
      description = "This package contains the generics system described in the\n/Scrap Your Boilerplate/ papers (see\n<http://www.cs.uu.nl/wiki/GenericProgramming/SYB>).\nIt defines the @Data@ class of types permitting folding and unfolding\nof constructor applications, instances of this class for primitive\ntypes, and a variety of traversals.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/syb-0.7.2.1.tar.gz";
      sha256 = "1807c66f77e66786739387f0ae9f16d150d1cfa9d626afcb729f0e9b442a8d96";
      });
    }) // {
    package-description-override = "name:                 syb\nversion:              0.7.2.1\nlicense:              BSD3\nlicense-file:         LICENSE\nauthor:               Ralf Lammel, Simon Peyton Jones, Jose Pedro Magalhaes\nmaintainer:           Sergey Vinokurov <serg.foo@gmail.com>\nhomepage:             http://www.cs.uu.nl/wiki/GenericProgramming/SYB\nbug-reports:          https://github.com/dreixel/syb/issues\nsynopsis:             Scrap Your Boilerplate\ndescription:\n    This package contains the generics system described in the\n    /Scrap Your Boilerplate/ papers (see\n    <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>).\n    It defines the @Data@ class of types permitting folding and unfolding\n    of constructor applications, instances of this class for primitive\n    types, and a variety of traversals.\n\ncategory:               Generics\nstability:              provisional\nbuild-type:             Simple\ncabal-version:          >= 1.10\ntested-with:            GHC==8.10.3, GHC==8.8.4, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2, GHC==7.2.2, GHC==7.0.4\n\nextra-source-files:     README.md,\n                        ChangeLog\n\nsource-repository head\n  type:                 git\n  location:             https://github.com/dreixel/syb\n\nLibrary\n  hs-source-dirs:         src\n  default-language:       Haskell98\n  build-depends:          base >= 4.0 && < 5.0\n  exposed-modules:        Data.Generics,\n                          Data.Generics.Basics,\n                          Data.Generics.Instances,\n                          Data.Generics.Aliases,\n                          Data.Generics.Schemes,\n                          Data.Generics.Text,\n                          Data.Generics.Twins,\n                          Data.Generics.Builders,\n\n                          Generics.SYB,\n                          Generics.SYB.Basics,\n                          Generics.SYB.Instances,\n                          Generics.SYB.Aliases,\n                          Generics.SYB.Schemes,\n                          Generics.SYB.Text,\n                          Generics.SYB.Twins,\n                          Generics.SYB.Builders\n\n  if impl(ghc < 6.12)\n    ghc-options:          -package-name syb\n\n  ghc-options:            -Wall\n\ntest-suite unit-tests\n  type:                   exitcode-stdio-1.0\n  hs-source-dirs:         tests\n  default-language:       Haskell98\n  main-is:                Main.hs\n  build-depends:          base\n                        , syb\n                        , tasty\n                        , tasty-hunit\n                        , containers\n                        , mtl\n  other-modules:          Bits\n                          Builders\n                          CompanyDatatypes\n                          Datatype\n                          Encode\n                          Ext\n                          Ext1\n                          Ext2\n                          FoldTree\n                          FreeNames\n                          GEq\n                          GMapQAssoc\n                          GRead\n                          GRead2\n                          GShow\n                          GShow2\n                          GZip\n                          GenUpTo\n                          GetC\n                          HList\n                          HOPat\n                          Labels\n                          LocalQuantors\n                          NestedDatatypes\n                          Newtype\n                          Paradise\n                          Perm\n                          Polymatch\n                          Reify\n                          Strings\n                          Tree\n                          Twin\n                          Typecase1\n                          Typecase2\n                          Where\n                          XML\n";
    }