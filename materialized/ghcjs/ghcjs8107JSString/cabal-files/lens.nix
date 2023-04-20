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
      benchmark-uniplate = false;
      inlining = true;
      old-inline-pragmas = false;
      dump-splices = false;
      test-doctests = true;
      test-hunit = true;
      test-properties = true;
      test-templates = true;
      safe = false;
      trustworthy = true;
      j = false;
      };
    package = {
      specVersion = "1.18";
      identifier = { name = "lens"; version = "4.19.2"; };
      license = "BSD-2-Clause";
      copyright = "Copyright (C) 2012-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/lens/";
      url = "";
      synopsis = "Lenses, Folds and Traversals";
      description = "This package comes \\\"Batteries Included\\\" with many useful lenses for the types\ncommonly used from the Haskell Platform, and with tools for automatically\ngenerating lenses and isomorphisms for user-supplied data types.\n\nThe combinators in @Control.Lens@ provide a highly generic toolbox for composing\nfamilies of getters, folds, isomorphisms, traversals, setters and lenses and their\nindexed variants.\n\nAn overview, with a large number of examples can be found in the <https://github.com/ekmett/lens#lens-lenses-folds-and-traversals README>.\n\nAn introductory video on the style of code used in this library by Simon Peyton Jones is available from <http://skillsmatter.com/podcast/scala/lenses-compositional-data-access-and-manipulation Skills Matter>.\n\nA video on how to use lenses and how they are constructed is available on <http://youtu.be/cefnmjtAolY?hd=1 youtube>.\n\nSlides for that second talk can be obtained from <http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf comonad.com>.\n\nMore information on the care and feeding of lenses, including a brief tutorial and motivation\nfor their types can be found on the <https://github.com/ekmett/lens/wiki lens wiki>.\n\nA small game of @pong@ and other more complex examples that manage their state using lenses can be found in the <https://github.com/ekmett/lens/blob/master/examples/ example folder>.\n\n/Lenses, Folds and Traversals/\n\nWith some signatures simplified, the core of the hierarchy of lens-like constructions looks like:\n\n\n<<http://i.imgur.com/ALlbPRa.png>>\n\n<images/Hierarchy.png (Local Copy)>\n\nYou can compose any two elements of the hierarchy above using @(.)@ from the @Prelude@, and you can\nuse any element of the hierarchy as any type it linked to above it.\n\nThe result is their lowest upper bound in the hierarchy (or an error if that bound doesn't exist).\n\nFor instance:\n\n* You can use any 'Traversal' as a 'Fold' or as a 'Setter'.\n\n* The composition of a 'Traversal' and a 'Getter' yields a 'Fold'.\n\n/Minimizing Dependencies/\n\nIf you want to provide lenses and traversals for your own types in your own libraries, then you\ncan do so without incurring a dependency on this (or any other) lens package at all.\n\n/e.g./ for a data type:\n\n> data Foo a = Foo Int Int a\n\nYou can define lenses such as\n\n> -- bar :: Lens' (Foo a) Int\n> bar :: Functor f => (Int -> f Int) -> Foo a -> f (Foo a)\n> bar f (Foo a b c) = fmap (\\a' -> Foo a' b c) (f a)\n\n> -- quux :: Lens (Foo a) (Foo b) a b\n> quux :: Functor f => (a -> f b) -> Foo a -> f (Foo b)\n> quux f (Foo a b c) = fmap (Foo a b) (f c)\n\nwithout the need to use any type that isn't already defined in the @Prelude@.\n\nAnd you can define a traversal of multiple fields with 'Control.Applicative.Applicative':\n\n> -- traverseBarAndBaz :: Traversal' (Foo a) Int\n> traverseBarAndBaz :: Applicative f => (Int -> f Int) -> Foo a -> f (Foo a)\n> traverseBarAndBaz f (Foo a b c) = Foo <$> f a <*> f b <*> pure c\n\nWhat is provided in this library is a number of stock lenses and traversals for\ncommon haskell types, a wide array of combinators for working them, and more\nexotic functionality, (/e.g./ getters, setters, indexed folds, isomorphisms).";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (errorHandler.setupDepError "filepath")))
        ];
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."kan-extensions" or (errorHandler.buildDepError "kan-extensions"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."type-equality" or (errorHandler.buildDepError "type-equality"));
        buildable = true;
        };
      tests = {
        "templates" = {
          depends = (pkgs.lib).optionals (!(!flags.test-templates)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            ];
          buildable = if !flags.test-templates then false else true;
          };
        "properties" = {
          depends = (pkgs.lib).optionals (!(!flags.test-properties)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = if !flags.test-properties then false else true;
          };
        "hunit" = {
          depends = (pkgs.lib).optionals (!(!flags.test-hunit)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = if !flags.test-hunit then false else true;
          };
        "doctests" = {
          depends = (pkgs.lib).optionals (!(!flags.test-doctests)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."simple-reflect" or (errorHandler.buildDepError "simple-reflect"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = if !flags.test-doctests then false else true;
          };
        };
      benchmarks = {
        "plated" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (flags.benchmark-uniplate) (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"));
          buildable = true;
          };
        "alongside" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        "folds" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            ];
          buildable = true;
          };
        "traversals" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            ];
          buildable = true;
          };
        "unsafe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lens-4.19.2.tar.gz";
      sha256 = "52f858ae3971a5104cdba5e81a27d154fda11fe65a54a4ac328c85904bdec23b";
      });
    }) // {
    package-description-override = "name:          lens\r\ncategory:      Data, Lenses, Generics\r\nversion:       4.19.2\r\nx-revision: 5\r\nlicense:       BSD2\r\ncabal-version: 1.18\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/lens/\r\nbug-reports:   http://github.com/ekmett/lens/issues\r\ncopyright:     Copyright (C) 2012-2016 Edward A. Kmett\r\nbuild-type:    Custom\r\n-- build-tools:   cpphs\r\ntested-with:   GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.3\r\n             , GHC == 8.10.1\r\nsynopsis:      Lenses, Folds and Traversals\r\ndescription:\r\n  This package comes \\\"Batteries Included\\\" with many useful lenses for the types\r\n  commonly used from the Haskell Platform, and with tools for automatically\r\n  generating lenses and isomorphisms for user-supplied data types.\r\n  .\r\n  The combinators in @Control.Lens@ provide a highly generic toolbox for composing\r\n  families of getters, folds, isomorphisms, traversals, setters and lenses and their\r\n  indexed variants.\r\n  .\r\n  An overview, with a large number of examples can be found in the <https://github.com/ekmett/lens#lens-lenses-folds-and-traversals README>.\r\n  .\r\n  An introductory video on the style of code used in this library by Simon Peyton Jones is available from <http://skillsmatter.com/podcast/scala/lenses-compositional-data-access-and-manipulation Skills Matter>.\r\n  .\r\n  A video on how to use lenses and how they are constructed is available on <http://youtu.be/cefnmjtAolY?hd=1 youtube>.\r\n  .\r\n  Slides for that second talk can be obtained from <http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf comonad.com>.\r\n  .\r\n  More information on the care and feeding of lenses, including a brief tutorial and motivation\r\n  for their types can be found on the <https://github.com/ekmett/lens/wiki lens wiki>.\r\n  .\r\n  A small game of @pong@ and other more complex examples that manage their state using lenses can be found in the <https://github.com/ekmett/lens/blob/master/examples/ example folder>.\r\n  .\r\n  /Lenses, Folds and Traversals/\r\n  .\r\n  With some signatures simplified, the core of the hierarchy of lens-like constructions looks like:\r\n  .\r\n  .\r\n  <<http://i.imgur.com/ALlbPRa.png>>\r\n  .\r\n  <images/Hierarchy.png (Local Copy)>\r\n  .\r\n  You can compose any two elements of the hierarchy above using @(.)@ from the @Prelude@, and you can\r\n  use any element of the hierarchy as any type it linked to above it.\r\n  .\r\n  The result is their lowest upper bound in the hierarchy (or an error if that bound doesn't exist).\r\n  .\r\n  For instance:\r\n  .\r\n  * You can use any 'Traversal' as a 'Fold' or as a 'Setter'.\r\n  .\r\n  * The composition of a 'Traversal' and a 'Getter' yields a 'Fold'.\r\n  .\r\n  /Minimizing Dependencies/\r\n  .\r\n  If you want to provide lenses and traversals for your own types in your own libraries, then you\r\n  can do so without incurring a dependency on this (or any other) lens package at all.\r\n  .\r\n  /e.g./ for a data type:\r\n  .\r\n  > data Foo a = Foo Int Int a\r\n  .\r\n  You can define lenses such as\r\n  .\r\n  > -- bar :: Lens' (Foo a) Int\r\n  > bar :: Functor f => (Int -> f Int) -> Foo a -> f (Foo a)\r\n  > bar f (Foo a b c) = fmap (\\a' -> Foo a' b c) (f a)\r\n  .\r\n  > -- quux :: Lens (Foo a) (Foo b) a b\r\n  > quux :: Functor f => (a -> f b) -> Foo a -> f (Foo b)\r\n  > quux f (Foo a b c) = fmap (Foo a b) (f c)\r\n  .\r\n  without the need to use any type that isn't already defined in the @Prelude@.\r\n  .\r\n  And you can define a traversal of multiple fields with 'Control.Applicative.Applicative':\r\n  .\r\n  > -- traverseBarAndBaz :: Traversal' (Foo a) Int\r\n  > traverseBarAndBaz :: Applicative f => (Int -> f Int) -> Foo a -> f (Foo a)\r\n  > traverseBarAndBaz f (Foo a b c) = Foo <$> f a <*> f b <*> pure c\r\n  .\r\n  What is provided in this library is a number of stock lenses and traversals for\r\n  common haskell types, a wide array of combinators for working them, and more\r\n  exotic functionality, (/e.g./ getters, setters, indexed folds, isomorphisms).\r\n\r\nextra-source-files:\r\n  .travis.yml\r\n  .gitignore\r\n  .hlint.yaml\r\n  .vim.custom\r\n  cabal.project\r\n  examples/LICENSE\r\n  examples/lens-examples.cabal\r\n  examples/*.hs\r\n  examples/*.lhs\r\n  examples/.hlint.yaml\r\n  include/*.h\r\n  lens-properties/.hlint.yaml\r\n  lens-properties/CHANGELOG.markdown\r\n  lens-properties/LICENSE\r\n  lens-properties/Setup.hs\r\n  lens-properties/lens-properties.cabal\r\n  travis/cabal-apt-install\r\n  travis/config\r\n  Warning.hs\r\n  AUTHORS.markdown\r\n  CHANGELOG.markdown\r\n  README.markdown\r\n  SUPPORT.markdown\r\nextra-doc-files:\r\n  images/*.png\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/ekmett/lens.git\r\n\r\ncustom-setup\r\n  setup-depends:\r\n    Cabal >= 1.10 && <3.3,\r\n    base  >= 4.5 && <5,\r\n    cabal-doctest >= 1 && <1.1,\r\n    filepath\r\n\r\n-- Enable benchmarking against Neil Mitchell's uniplate library for comparative performance analysis. Defaults to being turned off to avoid\r\n-- the extra dependency.\r\n--\r\n-- > cabal configure --enable-benchmarks -fbenchmark-uniplate && cabal build && cabal bench\r\nflag benchmark-uniplate\r\n  default: False\r\n  manual: True\r\n\r\n-- Generate inline pragmas when using template-haskell. This defaults to enabled, but you can\r\n--\r\n-- > cabal install lens -f-inlining\r\n--\r\n-- to shut it off to benchmark the relative performance impact, or as last ditch effort to address compile\r\n-- errors resulting from the myriad versions of template-haskell that all purport to be 2.8.\r\nflag inlining\r\n  manual: True\r\n  default: True\r\n\r\n-- Some 7.6.1-rc1 users report their TH still uses old style inline pragmas. This lets them turn on inlining.\r\nflag old-inline-pragmas\r\n  default: False\r\n  manual: True\r\n\r\n-- Make the test suites dump their template-haskell splices.\r\nflag dump-splices\r\n  default: False\r\n  manual: True\r\n\r\n-- You can disable the doctests test suite with -f-test-doctests\r\nflag test-doctests\r\n  default: True\r\n  manual: True\r\n\r\n-- You can disable the hunit test suite with -f-test-hunit\r\nflag test-hunit\r\n  default: True\r\n  manual: True\r\n\r\n-- Build the properties test if we're building tests\r\nflag test-properties\r\n  default: True\r\n  manual: True\r\n\r\nflag test-templates\r\n  default: True\r\n  manual: True\r\n\r\n-- Disallow unsafeCoerce\r\nflag safe\r\n  default: False\r\n  manual: True\r\n\r\n-- Assert that we are trustworthy when we can\r\nflag trustworthy\r\n  default: True\r\n  manual: True\r\n\r\n-- Attempt a parallel build with GHC 7.8\r\nflag j\r\n  default: False\r\n  manual: True\r\n\r\nlibrary\r\n  build-depends:\r\n    array                     >= 0.3.0.2  && < 0.6,\r\n    base                      >= 4.5      && < 5,\r\n    base-orphans              >= 0.5.2    && < 1,\r\n    bifunctors                >= 5.1      && < 6,\r\n    bytestring                >= 0.9.2.1  && < 0.11,\r\n    call-stack                >= 0.1      && < 0.4,\r\n    comonad                   >= 4        && < 6,\r\n    contravariant             >= 1.3      && < 2,\r\n    containers                >= 0.4.0    && < 0.7,\r\n    distributive              >= 0.3      && < 1,\r\n    filepath                  >= 1.2.0.0  && < 1.5,\r\n    free                      >= 4        && < 6,\r\n    ghc-prim,\r\n    hashable                  >= 1.1.2.3  && < 1.4,\r\n    kan-extensions            >= 5        && < 6,\r\n    exceptions                >= 0.1.1    && < 1,\r\n    mtl                       >= 2.0.1    && < 2.3,\r\n    parallel                  >= 3.1.0.1  && < 3.3,\r\n    profunctors               >= 5.2.1    && < 6,\r\n    reflection                >= 2.1      && < 3,\r\n    semigroupoids             >= 5        && < 6,\r\n    tagged                    >= 0.4.4    && < 1,\r\n    template-haskell          >= 2.4      && < 2.17,\r\n    th-abstraction            >= 0.3      && < 0.5,\r\n    text                      >= 0.11     && < 1.3,\r\n    transformers              >= 0.2      && < 0.6,\r\n    transformers-compat       >= 0.4      && < 1,\r\n    unordered-containers      >= 0.2.4    && < 0.3,\r\n    vector                    >= 0.9      && < 0.13\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:\r\n      generic-deriving >= 1.10  && < 2,\r\n      semigroups       >= 0.8.4 && < 1\r\n\r\n  if !impl(ghc >= 7.10)\r\n    build-depends:\r\n      nats >= 0.1 && < 1.2,\r\n      void >= 0.5 && < 1\r\n\r\n  if !impl(ghc >= 7.8)\r\n    build-depends:\r\n      type-equality >= 1 && < 2\r\n\r\n  exposed-modules:\r\n    Control.Exception.Lens\r\n    Control.Lens\r\n    Control.Lens.At\r\n    Control.Lens.Combinators\r\n    Control.Lens.Cons\r\n    Control.Lens.Each\r\n    Control.Lens.Empty\r\n    Control.Lens.Equality\r\n    Control.Lens.Extras\r\n    Control.Lens.Fold\r\n    Control.Lens.Getter\r\n    Control.Lens.Indexed\r\n    Control.Lens.Internal\r\n    Control.Lens.Internal.Bazaar\r\n    Control.Lens.Internal.ByteString\r\n    Control.Lens.Internal.Coerce\r\n    Control.Lens.Internal.Context\r\n    Control.Lens.Internal.CTypes\r\n    Control.Lens.Internal.Deque\r\n    Control.Lens.Internal.Exception\r\n    Control.Lens.Internal.FieldTH\r\n    Control.Lens.Internal.PrismTH\r\n    Control.Lens.Internal.Fold\r\n    Control.Lens.Internal.Getter\r\n    Control.Lens.Internal.Indexed\r\n    Control.Lens.Internal.Instances\r\n    Control.Lens.Internal.Iso\r\n    Control.Lens.Internal.Level\r\n    Control.Lens.Internal.List\r\n    Control.Lens.Internal.Magma\r\n    Control.Lens.Internal.Prism\r\n    Control.Lens.Internal.Review\r\n    Control.Lens.Internal.Setter\r\n    Control.Lens.Internal.TH\r\n    Control.Lens.Internal.Typeable\r\n    Control.Lens.Internal.Zoom\r\n    Control.Lens.Iso\r\n    Control.Lens.Lens\r\n    Control.Lens.Level\r\n    Control.Lens.Operators\r\n    Control.Lens.Plated\r\n    Control.Lens.Prism\r\n    Control.Lens.Reified\r\n    Control.Lens.Review\r\n    Control.Lens.Setter\r\n    Control.Lens.TH\r\n    Control.Lens.Traversal\r\n    Control.Lens.Tuple\r\n    Control.Lens.Type\r\n    Control.Lens.Unsound\r\n    Control.Lens.Wrapped\r\n    Control.Lens.Zoom\r\n    Control.Monad.Error.Lens\r\n    Control.Parallel.Strategies.Lens\r\n    Control.Seq.Lens\r\n    Data.Array.Lens\r\n    Data.Bits.Lens\r\n    Data.ByteString.Lens\r\n    Data.ByteString.Strict.Lens\r\n    Data.ByteString.Lazy.Lens\r\n    Data.Complex.Lens\r\n    Data.Data.Lens\r\n    Data.Dynamic.Lens\r\n    Data.HashSet.Lens\r\n    Data.IntSet.Lens\r\n    Data.List.Lens\r\n    Data.Map.Lens\r\n    Data.Sequence.Lens\r\n    Data.Set.Lens\r\n    Data.Text.Lens\r\n    Data.Text.Strict.Lens\r\n    Data.Text.Lazy.Lens\r\n    Data.Tree.Lens\r\n    Data.Typeable.Lens\r\n    Data.Vector.Lens\r\n    Data.Vector.Generic.Lens\r\n    GHC.Generics.Lens\r\n    System.Exit.Lens\r\n    System.FilePath.Lens\r\n    System.IO.Error.Lens\r\n    Language.Haskell.TH.Lens\r\n    Numeric.Lens\r\n    Numeric.Natural.Lens\r\n\r\n  other-modules:\r\n    Control.Lens.Internal.Prelude\r\n    Paths_lens\r\n\r\n  if flag(safe)\r\n    cpp-options: -DSAFE=1\r\n\r\n  if flag(trustworthy) && impl(ghc>=7.2)\r\n    other-extensions: Trustworthy\r\n    cpp-options: -DTRUSTWORTHY=1\r\n\r\n  if flag(old-inline-pragmas) && impl(ghc>=7.6.0.20120810)\r\n      cpp-options: -DOLD_INLINE_PRAGMAS=1\r\n\r\n  if flag(inlining)\r\n    cpp-options: -DINLINING\r\n\r\n  if impl(ghc<7.4)\r\n    ghc-options: -fno-spec-constr-count\r\n\r\n  if impl(ghc >= 7.10)\r\n    ghc-options: -fno-warn-trustworthy-safe\r\n\r\n  if impl(ghc >= 8)\r\n    ghc-options: -Wno-missing-pattern-synonym-signatures\r\n    ghc-options: -Wno-redundant-constraints\r\n\r\n  if flag(j) && impl(ghc>=7.8)\r\n    ghc-options: -j4\r\n\r\n  ghc-options: -Wall -fwarn-tabs -O2 -fdicts-cheap -funbox-strict-fields -fmax-simplifier-iterations=10\r\n\r\n  hs-source-dirs: src\r\n\r\n  include-dirs: include\r\n\r\n  default-language: Haskell2010\r\n\r\n-- Verify that Template Haskell expansion works\r\ntest-suite templates\r\n  type: exitcode-stdio-1.0\r\n  main-is: templates.hs\r\n  other-modules: T799\r\n  ghc-options: -Wall -threaded\r\n  hs-source-dirs: tests\r\n  default-language: Haskell2010\r\n\r\n  if flag(dump-splices)\r\n    ghc-options: -ddump-splices\r\n\r\n  if !flag(test-templates)\r\n    buildable: False\r\n  else\r\n    build-depends: base, lens\r\n\r\n-- Verify the properties of lenses with QuickCheck\r\ntest-suite properties\r\n  type: exitcode-stdio-1.0\r\n  main-is: properties.hs\r\n  other-modules:\r\n    Control.Lens.Properties\r\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\r\n  hs-source-dirs:\r\n    tests\r\n    lens-properties/src\r\n  include-dirs: include\r\n  default-language: Haskell2010\r\n  if !flag(test-properties)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n      base,\r\n      lens,\r\n      QuickCheck                 >= 2.4,\r\n      test-framework             >= 0.6,\r\n      test-framework-quickcheck2 >= 0.2,\r\n      transformers\r\n\r\ntest-suite hunit\r\n  type: exitcode-stdio-1.0\r\n  main-is: hunit.hs\r\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\r\n  hs-source-dirs: tests\r\n  default-language: Haskell2010\r\n\r\n  if !flag(test-hunit)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n      base,\r\n      containers,\r\n      HUnit >= 1.2,\r\n      lens,\r\n      mtl,\r\n      test-framework       >= 0.6,\r\n      test-framework-hunit >= 0.2\r\n\r\n-- Verify the results of the examples\r\ntest-suite doctests\r\n  type:              exitcode-stdio-1.0\r\n  main-is:           doctests.hs\r\n  ghc-options:       -Wall -threaded\r\n  hs-source-dirs:    tests\r\n  default-language:  Haskell2010\r\n  x-doctest-options: --fast\r\n\r\n  if flag(trustworthy) && impl(ghc>=7.2)\r\n    other-extensions: Trustworthy\r\n    cpp-options: -DTRUSTWORTHY=1\r\n\r\n  if !flag(test-doctests)\r\n    buildable: False\r\n  else\r\n    build-depends:\r\n      base,\r\n      bytestring,\r\n      containers,\r\n      directory      >= 1.0,\r\n      deepseq,\r\n      doctest        >= 0.11.4 && < 0.12 || >= 0.13 && < 0.19,\r\n      filepath,\r\n      generic-deriving,\r\n      lens,\r\n      mtl,\r\n      nats,\r\n      parallel,\r\n      semigroups     >= 0.9,\r\n      simple-reflect >= 0.3.1,\r\n      text,\r\n      unordered-containers,\r\n      vector < 0.12.2\r\n\r\n-- Basic benchmarks for the uniplate-style combinators\r\nbenchmark plated\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          plated.hs\r\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\r\n  hs-source-dirs:   benchmarks\r\n  default-language: Haskell2010\r\n  build-depends:\r\n    base,\r\n    base-compat >=0.11.0 && <0.12,\r\n    comonad,\r\n    criterion,\r\n    deepseq,\r\n    generic-deriving,\r\n    lens,\r\n    transformers\r\n\r\n  if flag(benchmark-uniplate)\r\n    build-depends: uniplate >= 1.6.7 && < 1.7\r\n    cpp-options: -DBENCHMARK_UNIPLATE\r\n\r\n-- Benchmarking alongside variants\r\nbenchmark alongside\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          alongside.hs\r\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\r\n  hs-source-dirs:   benchmarks\r\n  default-language: Haskell2010\r\n  build-depends:\r\n    base,\r\n    comonad >= 4,\r\n    criterion,\r\n    deepseq,\r\n    lens,\r\n    transformers\r\n\r\n-- Benchmarking folds\r\nbenchmark folds\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          folds.hs\r\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\r\n  hs-source-dirs:   benchmarks\r\n  default-language: Haskell2010\r\n  build-depends:\r\n    base,\r\n    criterion,\r\n    containers,\r\n    bytestring,\r\n    unordered-containers,\r\n    vector,\r\n    lens\r\n\r\n-- Benchmarking traversals\r\nbenchmark traversals\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          traversals.hs\r\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\r\n  hs-source-dirs:   benchmarks\r\n  default-language: Haskell2010\r\n  build-depends:\r\n    base,\r\n    criterion,\r\n    containers,\r\n    deepseq,\r\n    bytestring,\r\n    unordered-containers,\r\n    vector,\r\n    lens\r\n\r\n-- Benchmarking unsafe implementation strategies\r\nbenchmark unsafe\r\n  type:             exitcode-stdio-1.0\r\n  main-is:          unsafe.hs\r\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\r\n  hs-source-dirs:   benchmarks\r\n  default-language: Haskell2010\r\n  build-depends:\r\n    base,\r\n    comonad >= 4,\r\n    criterion >= 1,\r\n    deepseq,\r\n    generic-deriving,\r\n    lens,\r\n    transformers\r\n";
    }