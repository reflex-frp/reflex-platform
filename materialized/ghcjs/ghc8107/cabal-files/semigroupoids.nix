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
      containers = true;
      contravariant = true;
      distributive = true;
      doctests = true;
      comonad = true;
      tagged = true;
      unordered-containers = true;
      };
    package = {
      specVersion = "1.8";
      identifier = { name = "semigroupoids"; version = "5.3.4"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2011-2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/semigroupoids";
      url = "";
      synopsis = "Semigroupoids: Category sans id";
      description = "Provides a wide array of (semi)groupoids and operations for working with them.\n\nA 'Semigroupoid' is a 'Category' without the requirement of identity arrows for every object in the category.\n\nA 'Category' is any 'Semigroupoid' for which the Yoneda lemma holds.\n\nWhen working with comonads you often have the @\\<*\\>@ portion of an @Applicative@, but\nnot the @pure@. This was captured in Uustalu and Vene's \\\"Essence of Dataflow Programming\\\"\nin the form of the @ComonadZip@ class in the days before @Applicative@. Apply provides a weaker invariant, but for the comonads used for data flow programming (found in the streams package), this invariant is preserved. Applicative function composition forms a semigroupoid.\n\nSimilarly many structures are nearly a comonad, but not quite, for instance lists provide a reasonable 'extend' operation in the form of 'tails', but do not always contain a value.\n\nIdeally the following relationships would hold:\n\n> Foldable ----> Traversable <--- Functor ------> Alt ---------> Plus           Semigroupoid\n>      |               |            |                              |                  |\n>      v               v            v                              v                  v\n> Foldable1 ---> Traversable1     Apply --------> Applicative -> Alternative      Category\n>                                   |               |              |                  |\n>                                   v               v              v                  v\n>                                 Bind ---------> Monad -------> MonadPlus          Arrow\n>\n\nApply, Bind, and Extend (not shown) give rise the Static, Kleisli and Cokleisli semigroupoids respectively.\n\nThis lets us remove many of the restrictions from various monad transformers\nas in many cases the binding operation or @\\<*\\>@ operation does not require them.\n\nFinally, to work with these weaker structures it is beneficial to have containers\nthat can provide stronger guarantees about their contents, so versions of 'Traversable'\nand 'Foldable' that can be folded with just a 'Semigroup' are added.";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        ];
      };
    components = {
      "library" = {
        depends = (((((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.0" && (compiler.version).lt "7.2")) (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (flags.containers) (hsPkgs."containers" or (errorHandler.buildDepError "containers"))) ++ (pkgs.lib).optional (flags.contravariant) (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))) ++ (pkgs.lib).optional (flags.distributive) (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))) ++ (pkgs.lib).optional (flags.comonad) (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))) ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optionals (flags.unordered-containers) [
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      tests = {
        "doctests" = {
          depends = (pkgs.lib).optionals (!(!flags.doctests)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
            ];
          buildable = if !flags.doctests then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/semigroupoids-5.3.4.tar.gz";
      sha256 = "00d2e48973c3ab0a5d52616728ed63d0509454c8328148f698720014d7c58964";
      });
    }) // {
    package-description-override = "name:          semigroupoids\r\ncategory:      Control, Comonads\r\nversion:       5.3.4\r\nx-revision: 2\r\nlicense:       BSD3\r\ncabal-version: >= 1.8\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/semigroupoids\r\nbug-reports:   http://github.com/ekmett/semigroupoids/issues\r\ncopyright:     Copyright (C) 2011-2015 Edward A. Kmett\r\ntested-with:   GHC == 7.0.4\r\n             , GHC == 7.2.2\r\n             , GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.1\r\nbuild-type:    Custom\r\nsynopsis:      Semigroupoids: Category sans id\r\nextra-source-files:\r\n  .travis.yml\r\n  .gitignore\r\n  .vim.custom\r\n  README.markdown\r\n  CHANGELOG.markdown\r\n  Warning.hs\r\ndescription:\r\n  Provides a wide array of (semi)groupoids and operations for working with them.\r\n  .\r\n  A 'Semigroupoid' is a 'Category' without the requirement of identity arrows for every object in the category.\r\n  .\r\n  A 'Category' is any 'Semigroupoid' for which the Yoneda lemma holds.\r\n  .\r\n  When working with comonads you often have the @\\<*\\>@ portion of an @Applicative@, but\r\n  not the @pure@. This was captured in Uustalu and Vene's \\\"Essence of Dataflow Programming\\\"\r\n  in the form of the @ComonadZip@ class in the days before @Applicative@. Apply provides a weaker invariant, but for the comonads used for data flow programming (found in the streams package), this invariant is preserved. Applicative function composition forms a semigroupoid.\r\n  .\r\n  Similarly many structures are nearly a comonad, but not quite, for instance lists provide a reasonable 'extend' operation in the form of 'tails', but do not always contain a value.\r\n  .\r\n  Ideally the following relationships would hold:\r\n  .\r\n  > Foldable ----> Traversable <--- Functor ------> Alt ---------> Plus           Semigroupoid\r\n  >      |               |            |                              |                  |\r\n  >      v               v            v                              v                  v\r\n  > Foldable1 ---> Traversable1     Apply --------> Applicative -> Alternative      Category\r\n  >                                   |               |              |                  |\r\n  >                                   v               v              v                  v\r\n  >                                 Bind ---------> Monad -------> MonadPlus          Arrow\r\n  >\r\n  .\r\n  Apply, Bind, and Extend (not shown) give rise the Static, Kleisli and Cokleisli semigroupoids respectively.\r\n  .\r\n  This lets us remove many of the restrictions from various monad transformers\r\n  as in many cases the binding operation or @\\<*\\>@ operation does not require them.\r\n  .\r\n  Finally, to work with these weaker structures it is beneficial to have containers\r\n  that can provide stronger guarantees about their contents, so versions of 'Traversable'\r\n  and 'Foldable' that can be folded with just a 'Semigroup' are added.\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/semigroupoids.git\r\n\r\ncustom-setup\r\n  setup-depends:\r\n    base          >= 4 && < 5,\r\n    Cabal,\r\n    cabal-doctest >= 1 && < 1.1\r\n\r\nflag containers\r\n  description:\r\n    You can disable the use of the `containers` package using `-f-containers`.\r\n    .\r\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n  default: True\r\n  manual: True\r\n\r\nflag contravariant\r\n  description:\r\n    You can disable the use of the `contravariant` package using `-f-contravariant`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n    .\r\n    If disabled we will not supply instances of `Contravariant`\r\n    .\r\n  default: True\r\n  manual: True\r\n\r\nflag distributive\r\n  description:\r\n    You can disable the use of the `distributive` package using `-f-distributive`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n    .\r\n    If disabled we will not supply instances of `Distributive`\r\n    .\r\n  default: True\r\n  manual: True\r\n\r\nflag doctests\r\n  description:\r\n    You can disable testing with doctests using `-f-doctests`.\r\n  default: True\r\n  manual: True\r\n\r\nflag comonad\r\n  description:\r\n    You can disable the use of the `comonad` package using `-f-comonad`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n    .\r\n    If disabled we will not supply instances of `Comonad`\r\n    .\r\n  default: True\r\n  manual: True\r\n\r\nflag tagged\r\n  description:\r\n    You can disable the use of the `tagged` package using `-f-tagged`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n  default: True\r\n  manual: True\r\n\r\nflag unordered-containers\r\n  description:\r\n    You can disable the use of the `unordered-containers` package (and also its dependency `hashable`) using `-f-unordered-containers`.\r\n    .\r\n    Disabling this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\r\n  default: True\r\n  manual: True\r\n\r\nlibrary\r\n  build-depends:\r\n    base                >= 4.3     && < 5,\r\n    base-orphans        >= 0.8     && < 1,\r\n    bifunctors          >= 5       && < 6,\r\n    template-haskell,\r\n    transformers        >= 0.2     && < 0.6,\r\n    transformers-compat >= 0.5     && < 0.7\r\n\r\n  if impl(ghc >= 7.0 && < 7.2)\r\n    build-depends: generic-deriving >= 1.11 && < 1.15\r\n\r\n  if impl(ghc >= 7.2 && < 7.6)\r\n    build-depends: ghc-prim\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends: semigroups >= 0.16.2 && < 1\r\n\r\n  if flag(containers)\r\n    build-depends: containers >= 0.3 && < 0.7\r\n\r\n  if flag(contravariant)\r\n    build-depends: contravariant >= 0.2.0.1 && < 2\r\n\r\n  if flag(distributive)\r\n    build-depends: distributive >= 0.2.2 && < 1\r\n\r\n  if flag(comonad)\r\n    build-depends: comonad >= 4.2.6 && < 6\r\n\r\n  if flag(tagged)\r\n    build-depends: tagged >= 0.8.5 && < 1\r\n\r\n  if flag(unordered-containers)\r\n    build-depends: hashable >= 1.1  && < 1.4,\r\n                   unordered-containers >= 0.2  && < 0.3\r\n\r\n  hs-source-dirs: src\r\n\r\n  exposed-modules:\r\n    Data.Bifunctor.Apply\r\n    Data.Functor.Alt\r\n    Data.Functor.Apply\r\n    Data.Functor.Bind\r\n    Data.Functor.Bind.Class\r\n    Data.Functor.Bind.Trans\r\n    Data.Functor.Extend\r\n    Data.Functor.Plus\r\n    Data.Groupoid\r\n    Data.Isomorphism\r\n    Data.Semigroup.Bifoldable\r\n    Data.Semigroup.Bitraversable\r\n    Data.Semigroup.Foldable\r\n    Data.Semigroup.Foldable.Class\r\n    Data.Semigroup.Traversable\r\n    Data.Semigroup.Traversable.Class\r\n    Data.Semigroupoid\r\n    Data.Semigroupoid.Dual\r\n    Data.Semigroupoid.Ob\r\n    Data.Semigroupoid.Static\r\n    Data.Traversable.Instances\r\n\r\n  ghc-options: -Wall -fno-warn-warnings-deprecations\r\n\r\n  if impl(ghc >= 7.10)\r\n    ghc-options: -fno-warn-trustworthy-safe\r\n\r\ntest-suite doctests\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          doctests.hs\r\n  hs-source-dirs:   test\r\n  ghc-options:      -Wall -fno-warn-warnings-deprecations\r\n\r\n  if !flag(doctests)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n      base      >= 4      && < 5,\r\n      doctest   >= 0.11.1 && < 0.18,\r\n      semigroupoids\r\n";
    }