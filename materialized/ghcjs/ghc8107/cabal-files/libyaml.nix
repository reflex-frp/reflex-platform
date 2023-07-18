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
    flags = { no-unicode = false; system-libyaml = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "libyaml"; version = "0.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov";
      homepage = "https://github.com/snoyberg/yaml#readme";
      url = "";
      synopsis = "Low-level, streaming YAML interface.";
      description = "README and API documentation are available at <https://www.stackage.org/package/libyaml>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."directory" or (errorHandler.buildDepError "directory"));
        libs = (pkgs.lib).optional (!(!flags.system-libyaml)) (pkgs."yaml" or (errorHandler.sysDepError "yaml"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/libyaml-0.1.2.tar.gz";
      sha256 = "8f42d66f199fcaee255326f8f770d88b0670df56b5eb78002d6058f3a45e97b5";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.31.2.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 93d917f62be86415287d10db638b1d5422a21b7a4c5b229fbe16b62c47717555\n\nname:           libyaml\nversion:        0.1.2\nsynopsis:       Low-level, streaming YAML interface.\ndescription:    README and API documentation are available at <https://www.stackage.org/package/libyaml>\ncategory:       Text\nstability:      stable\nhomepage:       https://github.com/snoyberg/yaml#readme\nbug-reports:    https://github.com/snoyberg/yaml/issues\nauthor:         Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov\nmaintainer:     Michael Snoyman <michael@snoyman.com>\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    c/helper.h\n    libyaml_src/yaml_private.h\n    libyaml_src/yaml.h\n    libyaml_src/LICENSE\n    README.md\n    ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/snoyberg/yaml\n\nflag no-unicode\n  description: Don't enable unicode output. Instead, unicode characters will be escaped.\n  manual: False\n  default: False\n\nflag system-libyaml\n  description: Use the system-wide libyaml instead of the bundled copy\n  manual: False\n  default: False\n\nlibrary\n  exposed-modules:\n      Text.Libyaml\n  other-modules:\n      Paths_libyaml\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  include-dirs:\n      c\n  c-sources:\n      c/helper.c\n  build-depends:\n      base >=4.9.1 && <5\n    , bytestring >=0.9.1.4\n    , conduit >=1.2.8 && <1.4\n    , resourcet >=0.3 && <1.3\n  if flag(no-unicode)\n    cpp-options: -D__NO_UNICODE__\n  if !(flag(system-libyaml))\n    include-dirs:\n        libyaml_src\n    c-sources:\n        libyaml_src/api.c\n        libyaml_src/dumper.c\n        libyaml_src/emitter.c\n        libyaml_src/loader.c\n        libyaml_src/parser.c\n        libyaml_src/reader.c\n        libyaml_src/scanner.c\n        libyaml_src/writer.c\n  else\n    extra-libraries:\n        yaml\n  if os(windows)\n    cpp-options: -DWINDOWS\n    build-depends:\n        directory\n  default-language: Haskell2010\n";
    }