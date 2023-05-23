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
    flags = { no-examples = true; no-exe = true; };
    package = {
      specVersion = "1.12";
      identifier = { name = "yaml"; version = "0.11.5.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov";
      homepage = "https://github.com/snoyberg/yaml#readme";
      url = "";
      synopsis = "Support for parsing and rendering YAML documents.";
      description = "README and API documentation are available at <https://www.stackage.org/package/yaml>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      exes = {
        "examples" = {
          depends = ([
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optionals (!flags.no-examples) [
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = if flags.no-examples then false else true;
          };
        "json2yaml" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = if flags.no-exe then false else true;
          };
        "yaml2json" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = if flags.no-exe then false else true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            (hsPkgs."mockery" or (errorHandler.buildDepError "mockery"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/yaml-0.11.5.0.tar.gz";
      sha256 = "b28e748bd69948cb1b43694d4d7c74756e060e09ca91688d0485e23f19d6cdad";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.33.0.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 55c85c8d4d3074a558a82e30a2592ecff9db2e6f1571547c73d26ba44bfc1c20\n\nname:           yaml\nversion:        0.11.5.0\nsynopsis:       Support for parsing and rendering YAML documents.\ndescription:    README and API documentation are available at <https://www.stackage.org/package/yaml>\ncategory:       Data\nstability:      stable\nhomepage:       https://github.com/snoyberg/yaml#readme\nbug-reports:    https://github.com/snoyberg/yaml/issues\nauthor:         Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov\nmaintainer:     Michael Snoyman <michael@snoyman.com>\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    test/largest-string.yaml\n    test/json.yaml\n    test/resources/foo.yaml\n    test/resources/bar.yaml\n    test/resources/baz.yaml\n    test/resources/accent/foo.yaml\n    test/resources/loop/foo.yaml\n    test/resources/loop/bar.yaml\n    test/resources/empty.yaml\n    test/resources/empty2.yaml\n    README.md\n    ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/snoyberg/yaml\n\nflag no-examples\n  description: don't build the examples\n  manual: False\n  default: True\n\nflag no-exe\n  description: don't install the yaml2json or json2yaml executables\n  manual: False\n  default: True\n\nlibrary\n  exposed-modules:\n      Data.Yaml\n      Data.Yaml.Aeson\n      Data.Yaml.Builder\n      Data.Yaml.Config\n      Data.Yaml.Include\n      Data.Yaml.Internal\n      Data.Yaml.Parser\n      Data.Yaml.Pretty\n      Data.Yaml.TH\n  other-modules:\n      Paths_yaml\n  hs-source-dirs:\n      src\n  other-extensions: LambdaCase\n  ghc-options: -Wall\n  build-depends:\n      aeson >=0.11\n    , attoparsec >=0.11.3.0\n    , base >=4.9.1 && <5\n    , bytestring >=0.9.1.4\n    , conduit >=1.2.8 && <1.4\n    , containers\n    , directory\n    , filepath\n    , libyaml >=0.1 && <0.2\n    , mtl\n    , resourcet >=0.3 && <1.3\n    , scientific >=0.3\n    , template-haskell\n    , text\n    , transformers >=0.1\n    , unordered-containers\n    , vector\n  if !impl(ghc >= 8.0)\n    build-depends:\n        semigroups\n  default-language: Haskell2010\n\nexecutable examples\n  main-is: Main.hs\n  other-modules:\n      Config\n      Simple\n      Paths_yaml\n  hs-source-dirs:\n      examples\n  ghc-options: -Wall\n  build-depends:\n      aeson >=0.11\n    , attoparsec >=0.11.3.0\n    , base >=4.9.1 && <5\n    , bytestring >=0.9.1.4\n    , conduit >=1.2.8 && <1.4\n    , containers\n    , directory\n    , filepath\n    , libyaml >=0.1 && <0.2\n    , mtl\n    , resourcet >=0.3 && <1.3\n    , scientific >=0.3\n    , template-haskell\n    , text\n    , transformers >=0.1\n    , unordered-containers\n    , vector\n  if !impl(ghc >= 8.0)\n    build-depends:\n        semigroups\n  if flag(no-examples)\n    buildable: False\n  else\n    build-depends:\n        raw-strings-qq\n      , yaml\n  default-language: Haskell2010\n\nexecutable json2yaml\n  main-is: json2yaml.hs\n  other-modules:\n      Paths_yaml\n  hs-source-dirs:\n      exe\n  build-depends:\n      aeson >=0.11\n    , attoparsec >=0.11.3.0\n    , base >=4.9.1 && <5\n    , bytestring >=0.9.1.4\n    , conduit >=1.2.8 && <1.4\n    , containers\n    , directory\n    , filepath\n    , libyaml >=0.1 && <0.2\n    , mtl\n    , resourcet >=0.3 && <1.3\n    , scientific >=0.3\n    , template-haskell\n    , text\n    , transformers >=0.1\n    , unordered-containers\n    , vector\n    , yaml\n  if !impl(ghc >= 8.0)\n    build-depends:\n        semigroups\n  if flag(no-exe)\n    buildable: False\n  default-language: Haskell2010\n\nexecutable yaml2json\n  main-is: yaml2json.hs\n  other-modules:\n      Paths_yaml\n  hs-source-dirs:\n      exe\n  build-depends:\n      aeson >=0.11\n    , attoparsec >=0.11.3.0\n    , base >=4.9.1 && <5\n    , bytestring >=0.9.1.4\n    , conduit >=1.2.8 && <1.4\n    , containers\n    , directory\n    , filepath\n    , libyaml >=0.1 && <0.2\n    , mtl\n    , resourcet >=0.3 && <1.3\n    , scientific >=0.3\n    , template-haskell\n    , text\n    , transformers >=0.1\n    , unordered-containers\n    , vector\n    , yaml\n  if !impl(ghc >= 8.0)\n    build-depends:\n        semigroups\n  if flag(no-exe)\n    buildable: False\n  default-language: Haskell2010\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Data.Yaml.IncludeSpec\n      Data.Yaml.THSpec\n      Data.YamlSpec\n      Paths_yaml\n  hs-source-dirs:\n      test\n  ghc-options: -Wall \"-with-rtsopts=-K1K\"\n  cpp-options: -DTEST\n  build-depends:\n      HUnit\n    , aeson >=0.11\n    , attoparsec >=0.11.3.0\n    , base >=4.9.1 && <5\n    , base-compat\n    , bytestring >=0.9.1.4\n    , conduit >=1.2.8 && <1.4\n    , containers\n    , directory\n    , filepath\n    , hspec >=1.3\n    , libyaml >=0.1 && <0.2\n    , mockery\n    , mtl\n    , raw-strings-qq\n    , resourcet >=0.3 && <1.3\n    , scientific >=0.3\n    , template-haskell\n    , temporary\n    , text\n    , transformers >=0.1\n    , unordered-containers\n    , vector\n    , yaml\n  if !impl(ghc >= 8.0)\n    build-depends:\n        semigroups\n  default-language: Haskell2010\n";
    }