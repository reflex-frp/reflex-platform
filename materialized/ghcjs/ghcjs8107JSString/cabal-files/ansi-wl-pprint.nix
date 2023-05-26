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
    flags = { example = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ansi-wl-pprint"; version = "0.6.9"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Edward Kmett <ekmett@gmail.com>";
      author = "Daan Leijen, Max Bolingbroke";
      homepage = "http://github.com/ekmett/ansi-wl-pprint";
      url = "";
      synopsis = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
      description = "This is a pretty printing library based on Wadler's paper [\"A Prettier Printer\"](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).\nIt has been enhanced with support for ANSI terminal colored output using the [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal) package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      exes = {
        "ansi-wl-pprint-example" = {
          depends = (pkgs.lib).optionals (flags.example) [
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            ];
          buildable = if flags.example then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ansi-wl-pprint-0.6.9.tar.gz";
      sha256 = "a7b2e8e7cd3f02f2954e8b17dc60a0ccd889f49e2068ebb15abfa1d42f7a4eac";
      });
    }) // {
    package-description-override = "cabal-version:       >= 1.10\nname:                ansi-wl-pprint\nversion:             0.6.9\nx-revision:          2\n\ncategory:            User Interfaces, Text\nsynopsis:            The Wadler/Leijen Pretty Printer for colored ANSI terminal output\ndescription:         {\n\nThis is a pretty printing library based on Wadler's paper [\"A Prettier Printer\"](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).\nIt has been enhanced with support for ANSI terminal colored output using the [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal) package.\n\n}\nlicense:             BSD3\nlicense-file:        LICENSE\nextra-source-files:  README.md Changelog.md\nauthor:              Daan Leijen, Max Bolingbroke\nmaintainer:          Edward Kmett <ekmett@gmail.com>\nbug-reports:         http://github.com/ekmett/ansi-wl-pprint/issues\nhomepage:            http://github.com/ekmett/ansi-wl-pprint\nbuild-type:          Simple\ntested-with:         GHC==7.0.2, GHC==7.2.2, GHC==7.4.2, GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.3, GHC==8.6.5, GHC==8.8.1\n\nsource-repository head\n  type: git\n  location: https://github.com/ekmett/ansi-wl-pprint.git\n\nflag Example\n  description:    Build the example application\n  default:        False\n  manual:         True\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs: .\n  exposed-modules: Text.PrettyPrint.ANSI.Leijen\n                 , Text.PrettyPrint.ANSI.Leijen.Internal\n  ghc-options: -Wall -fno-warn-name-shadowing -fno-warn-unused-matches\n\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n    if impl(ghc < 8.8)\n      ghc-options: -Wnoncanonical-monadfail-instances\n  else\n    -- see also notes in Text.PrettyPrint.ANSI.Leijen\n    build-depends: semigroups >= 0.18.5 && < 0.20\n\n  build-depends: ansi-terminal >= 0.9.1 && < 0.12\n  build-depends: base >= 4.3 && < 5\n\n  if impl(ghc >= 7.4)\n    default-extensions: Safe\n  else\n    if impl(ghc >= 7.2)\n      default-extensions: Trustworthy\n\nexecutable ansi-wl-pprint-example\n  default-language: Haskell2010\n  hs-source-dirs: src-exe\n  main-is: Example.hs\n\n  if flag(example)\n    build-depends: ansi-wl-pprint\n    -- dependencies whose constraints are inherited via lib:ansi-wl-pprint\n    build-depends: base, ansi-terminal\n  else\n    buildable: False\n";
    }