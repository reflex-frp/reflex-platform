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
      specVersion = "1.12";
      identifier = { name = "regex-base"; version = "0.94.0.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2006, Christopher Kuklewicz";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>,\nAndreas Abel";
      author = "Christopher Kuklewicz";
      homepage = "https://wiki.haskell.org/Regular_expressions";
      url = "";
      synopsis = "Common \"Text.Regex.*\" API for Regex matching";
      description = "This package does not provide the ability to do regular expression matching.\nInstead, it provides the type classes that constitute the abstract API\nthat is implemented by @regex-*@ backends such as:\n\n* <https://hackage.haskell.org/package/regex-posix regex-posix>\n\n* <https://hackage.haskell.org/package/regex-parsec regex-parsec>\n\n* <https://hackage.haskell.org/package/regex-dfa regex-dfa>\n\n* <https://hackage.haskell.org/package/regex-tdfa regex-tdfa>\n\n* <https://hackage.haskell.org/package/regex-pcre regex-pcre>\n\nSee also <https://wiki.haskell.org/Regular_expressions> for more information.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "7.4") [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/regex-base-0.94.0.1.tar.gz";
      sha256 = "71b1d96fff201f31fe8cd4532f056aca03a21cd486890256dc3007dd73adedd9";
      });
    }) // {
    package-description-override = "cabal-version:          1.12\nname:                   regex-base\nversion:                0.94.0.1\n\nbuild-type:             Simple\nlicense:                BSD3\nlicense-file:           LICENSE\ncopyright:              Copyright (c) 2006, Christopher Kuklewicz\nauthor:                 Christopher Kuklewicz\nmaintainer:\n  Herbert Valerio Riedel <hvr@gnu.org>,\n  Andreas Abel\nhomepage:               https://wiki.haskell.org/Regular_expressions\nbug-reports:            https://github.com/hvr/regex-base/issues\nsynopsis:               Common \"Text.Regex.*\" API for Regex matching\ncategory:               Text\ndescription:\n  This package does not provide the ability to do regular expression matching.\n  Instead, it provides the type classes that constitute the abstract API\n  that is implemented by @regex-*@ backends such as:\n  .\n  * <https://hackage.haskell.org/package/regex-posix regex-posix>\n  .\n  * <https://hackage.haskell.org/package/regex-parsec regex-parsec>\n  .\n  * <https://hackage.haskell.org/package/regex-dfa regex-dfa>\n  .\n  * <https://hackage.haskell.org/package/regex-tdfa regex-tdfa>\n  .\n  * <https://hackage.haskell.org/package/regex-pcre regex-pcre>\n  .\n  See also <https://wiki.haskell.org/Regular_expressions> for more information.\n\nextra-source-files:\n  ChangeLog.md\n\ntested-with:\n  -- Haskell CI:\n  GHC == 7.0.4\n  GHC == 7.2.2\n  GHC == 7.4.2\n  GHC == 7.6.3\n  GHC == 7.8.4\n  GHC == 7.10.3\n  GHC == 8.0.2\n  GHC == 8.2.2\n  GHC == 8.4.4\n  GHC == 8.6.5\n  GHC == 8.8.4\n  GHC == 8.10.3\n  -- manually (AA, 2021-02-16):\n  -- GHC == 8.10.4\n  -- GHC == 9.0.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/hvr/regex-base.git\n\nsource-repository this\n  type:     git\n  location: https://github.com/hvr/regex-base.git\n  tag:      v0.94.0.1\n\nlibrary\n  hs-source-dirs: src\n\n  exposed-modules:\n      Text.Regex.Base\n      Text.Regex.Base.RegexLike\n      Text.Regex.Base.Context\n      Text.Regex.Base.Impl\n\n  other-modules:\n      Paths_regex_base\n\n  default-language: Haskell2010\n  other-extensions:\n      MultiParamTypeClasses\n      FunctionalDependencies\n      TypeSynonymInstances\n      FlexibleInstances\n      FlexibleContexts\n\n  if impl(ghc >= 7.4)\n    default-extensions: Safe\n    build-depends: containers >= 0.4.2.1\n                 , bytestring >= 0.9.2.1\n\n  build-depends: base       >= 4.3 && < 4.16\n               , mtl        >= 1.1 && < 2.3\n               , containers >= 0.4 && < 0.7\n               , bytestring >= 0.9 && < 0.12\n               , array      >= 0.3 && < 0.6\n               , text       >= 1.2.3 && < 1.3\n\n  if !impl(ghc >= 8)\n      build-depends: fail == 4.9.*\n\n  ghc-options: -Wall\n";
    }