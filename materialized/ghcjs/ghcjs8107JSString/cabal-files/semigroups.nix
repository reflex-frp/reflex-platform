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
      hashable = true;
      binary = true;
      bytestring = true;
      bytestring-builder = false;
      containers = true;
      deepseq = true;
      tagged = true;
      template-haskell = true;
      text = true;
      transformers = true;
      unordered-containers = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "semigroups"; version = "0.19.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/semigroups/";
      url = "";
      synopsis = "Anything that associates";
      description = "In mathematics, a semigroup is an algebraic structure consisting of a set together with an associative binary operation. A semigroup generalizes a monoid in that there might not exist an identity element. It also (originally) generalized a group (a monoid with all inverses) to a type where every element did not have to have an inverse, thus the name semigroup.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "7.11.20151002") ((((((((((((pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."nats" or (errorHandler.buildDepError "nats")) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.5")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (flags.binary) (hsPkgs."binary" or (errorHandler.buildDepError "binary"))) ++ (pkgs.lib).optionals (flags.bytestring) (if flags.bytestring-builder
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (flags.containers) (hsPkgs."containers" or (errorHandler.buildDepError "containers"))) ++ (pkgs.lib).optional (flags.deepseq) (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))) ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (flags.text) (hsPkgs."text" or (errorHandler.buildDepError "text"))) ++ (pkgs.lib).optional (flags.hashable) (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))) ++ (pkgs.lib).optional (flags.hashable && flags.unordered-containers) (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))) ++ (pkgs.lib).optionals (flags.transformers) [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optional (flags.template-haskell) (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/semigroups-0.19.1.tar.gz";
      sha256 = "79e761e64b862564a3470d5d356cb6b060b14452d675859aed3b2d1e14646648";
      });
    }) // {
    package-description-override = "name:          semigroups\ncategory:      Algebra, Data, Data Structures, Math\nversion:       0.19.1\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/semigroups/\nbug-reports:   http://github.com/ekmett/semigroups/issues\ncopyright:     Copyright (C) 2011-2015 Edward A. Kmett\nsynopsis:      Anything that associates\ndescription:\n    In mathematics, a semigroup is an algebraic structure consisting of a set together with an associative binary operation. A semigroup generalizes a monoid in that there might not exist an identity element. It also (originally) generalized a group (a monoid with all inverses) to a type where every element did not have to have an inverse, thus the name semigroup.\nbuild-type:    Simple\nextra-source-files: .travis.yml README.markdown CHANGELOG.markdown\ntested-with:   GHC == 7.0.4\n             , GHC == 7.2.2\n             , GHC == 7.4.2\n             , GHC == 7.6.3\n             , GHC == 7.8.4\n             , GHC == 7.10.3\n             , GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.1\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/semigroups.git\n\nflag hashable\n  description:\n    You can disable the use of the `hashable` package using `-f-hashable`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n    .\n    If disabled we will not supply instances of `Hashable`\n    .\n    Note: `-f-hashable` implies `-f-unordered-containers`, as we are necessarily not able to supply those instances as well.\n  default: True\n  manual: True\n\nflag binary\n  description:\n    You can disable the use of the `binary` package using `-f-binary`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag bytestring\n  description:\n    You can disable the use of the `bytestring` package using `-f-bytestring`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag bytestring-builder\n  description:\n    Decides whether to use an older version of bytestring along with bytestring-builder or just a newer version of bytestring.\n    .\n    This flag normally toggles automatically but you can use `-fbytestring-builder` or `-f-bytestring-builder` to explicitly change it.\n  default: False\n  manual: False\n\nflag containers\n  description:\n    You can disable the use of the `containers` package using `-f-containers`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag deepseq\n  description:\n    You can disable the use of the `deepseq` package using `-f-deepseq`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag tagged\n  description:\n    You can disable the use of the `tagged` package using `-f-tagged`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag template-haskell\n  description:\n    You can disable the use of the `template-haskell` package using `-f-template-haskell`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag text\n  description:\n    You can disable the use of the `text` package using `-f-text`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag transformers\n  description:\n    You can disable the use of the `transformers` and `transformers-compat` packages using `-f-transformers`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nflag unordered-containers\n  description:\n    You can disable the use of the `unordered-containers` package using `-f-unordered-containers`.\n    .\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n  default: True\n  manual: True\n\nlibrary\n  default-language: Haskell98\n  hs-source-dirs: src\n  ghc-options: -Wall\n\n  build-depends: base >= 2 && < 5\n\n  if impl(ghc >= 7.2)\n    exposed-modules:\n      Data.Semigroup.Generic\n\n  -- legacy configuration\n  if impl(ghc < 7.11.20151002)\n    -- starting with GHC 8 these modules are provided by `base`\n    hs-source-dirs: src-ghc7\n    exposed-modules:\n      Data.Semigroup\n      Data.List.NonEmpty\n\n    -- Not needed anymore since GHC 7.10\n    if impl(ghc < 7.10)\n      build-depends: nats >= 0.1 && < 2\n\n    if impl(ghc >= 7.2 && < 7.5)\n      build-depends: ghc-prim\n\n    if flag(binary)\n      build-depends: binary\n\n    if flag(bytestring)\n      if flag(bytestring-builder)\n        build-depends: bytestring         >= 0.9    && < 0.10.4,\n                       bytestring-builder >= 0.10.4 && < 1\n      else\n        build-depends: bytestring         >= 0.10.4 && < 1\n\n    if flag(containers)\n      build-depends: containers >= 0.3 && < 0.7\n\n    if flag(deepseq)\n      build-depends: deepseq >= 1.1 && < 1.5\n\n    if flag(tagged)\n      build-depends: tagged >= 0.4.4 && < 1\n\n    if flag(text)\n      build-depends: text >= 0.10 && < 2\n\n    if flag(hashable)\n      build-depends: hashable >= 1.2.5.0  && < 1.4\n\n    if flag(hashable) && flag(unordered-containers)\n      build-depends: unordered-containers >= 0.2  && < 0.3\n\n    if flag(transformers)\n      build-depends: transformers        >= 0.2 && < 0.6\n                   , transformers-compat >= 0.5 && < 1\n\n    if flag(template-haskell)\n      build-depends: template-haskell >=2.5.0.0 && <2.11\n      other-modules: Paths_semigroups\n";
    }