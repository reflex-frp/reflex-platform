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
      identifier = { name = "type-equality"; version = "1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>, Ryan Scott <ryan.gl.scott@gmail.com>, Erik Hesselink <hesselink@gmail.com>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>, Ryan Scott <ryan.gl.scott@gmail.com>, Erik Hesselink <hesselink@gmail.com>, Martijn van Steenbergen";
      homepage = "https://github.com/hesselink/type-equality";
      url = "";
      synopsis = "Data.Type.Equality compat package";
      description = "This library defines a propositional equality data type,\nshims @Data.Type.Equality@ as well as possible for older GHCs (< 7.8).\n\n@\ndata a :~: b where\n\\    Refl :: a :~: a\n@\n\nThe module @Data.Type.Equality.Hetero@ shims @:~~:@ equality, for\ncompilers with @PolyKinds@";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/type-equality-1.tar.gz";
      sha256 = "4728b502a211454ef682a10d7a3e817c22d06ba509df114bb267ef9d43a08ce8";
      });
    }) // {
    package-description-override = "name:               type-equality\nversion:            1\nx-revision:         2\nstability:          provisional\ncabal-version:      >=1.10\nbuild-type:         Simple\nauthor:\n  Oleg Grenrus <oleg.grenrus@iki.fi>, Ryan Scott <ryan.gl.scott@gmail.com>, Erik Hesselink <hesselink@gmail.com>, Martijn van Steenbergen\n\nmaintainer:\n  Oleg Grenrus <oleg.grenrus@iki.fi>, Ryan Scott <ryan.gl.scott@gmail.com>, Erik Hesselink <hesselink@gmail.com>\n\nlicense:            BSD3\nlicense-file:       LICENSE\nhomepage:           https://github.com/hesselink/type-equality\ncategory:           Data, Dependent Types\nsynopsis:           Data.Type.Equality compat package\ndescription:\n  This library defines a propositional equality data type,\n  shims @Data.Type.Equality@ as well as possible for older GHCs (< 7.8).\n  .\n  @\n  data a :~: b where\n  \\    Refl :: a :~: a\n  @\n  .\n  The module @Data.Type.Equality.Hetero@ shims @:~~:@ equality, for\n  compilers with @PolyKinds@\n\nextra-source-files: CHANGELOG.md\ntested-with:\n  GHC ==7.0.4\n   || ==7.2.2\n   || ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.3\n\nsource-repository head\n  type:     git\n  location: git://github.com/hesselink/type-equality\n\nlibrary\n  default-language: Haskell2010\n  build-depends:    base >=4.3 && <4.16\n\n  if !impl(ghc >=7.8)\n    hs-source-dirs:  src-old\n    exposed-modules: Data.Type.Equality\n\n  if impl(ghc >=8.0)\n    hs-source-dirs:   src-hetero\n    exposed-modules:  Data.Type.Equality.Hetero\n    other-extensions: PolyKinds\n";
    }