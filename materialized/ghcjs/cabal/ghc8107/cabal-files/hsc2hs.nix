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
    flags = { in-ghc-tree = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "hsc2hs"; version = "0.68.8"; };
      license = "BSD-3-Clause";
      copyright = "2000, Marcin Kowalczyk";
      maintainer = "ghc-devs@haskell.org";
      author = "Marcin Kowalczyk <qrczak@knm.org.pl>";
      homepage = "";
      url = "";
      synopsis = "A preprocessor that helps with writing Haskell bindings to C code";
      description = "The hsc2hs program can be used to automate some parts of the\nprocess of writing Haskell bindings to C code.  It reads an\nalmost-Haskell source file with embedded special constructs, and\noutputs a real Haskell file with these constructs processed, based\non information taken from some C headers.  The extra constructs\nprovide Haskell counterparts of C types, values of C constants,\nincluding sizes of C types, and access to fields of C structs.\n\nFor more details, see the\n<http://downloads.haskell.org/~ghc/master/users-guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs hsc2hs section>\nin the GHC User's Guide.";
      buildType = "Simple";
      };
    components = {
      exes = {
        "hsc2hs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."process" or (errorHandler.buildDepError "process"));
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hsc2hs-0.68.8.tar.gz";
      sha256 = "78341efbc917a84a07f2143bee9203e2555072054d495717aa73d89d9df77a52";
      });
    }) // {
    package-description-override = "cabal-version: >=1.10\r\nName: hsc2hs\r\nVersion: 0.68.8\r\nx-revision: 1\r\n\r\nCopyright: 2000, Marcin Kowalczyk\r\nLicense: BSD3\r\nLicense-File: LICENSE\r\nAuthor: Marcin Kowalczyk <qrczak@knm.org.pl>\r\nMaintainer: ghc-devs@haskell.org\r\nSynopsis: A preprocessor that helps with writing Haskell bindings to C code\r\nBug-Reports: https://github.com/haskell/hsc2hs/issues\r\nDescription:\r\n    The hsc2hs program can be used to automate some parts of the\r\n    process of writing Haskell bindings to C code.  It reads an\r\n    almost-Haskell source file with embedded special constructs, and\r\n    outputs a real Haskell file with these constructs processed, based\r\n    on information taken from some C headers.  The extra constructs\r\n    provide Haskell counterparts of C types, values of C constants,\r\n    including sizes of C types, and access to fields of C structs.\r\n    .\r\n    For more details, see the\r\n    <http://downloads.haskell.org/~ghc/master/users-guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs hsc2hs section>\r\n    in the GHC User's Guide.\r\nCategory: Development\r\nData-Dir: data/\r\nData-Files: template-hsc.h\r\nbuild-type: Simple\r\ntested-with: GHC==8.10.1, GHC==8.8.3, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2, GHC==7.2.2, GHC==7.0.4\r\n\r\nextra-source-files:\r\n  changelog.md\r\n  test/asm/*.s\r\n\r\nflag in-ghc-tree\r\n  description: Are we in a GHC tree?\r\n  default: False\r\n  manual: True\r\n\r\nsource-repository head\r\n    Type: git\r\n    Location: https://github.com/haskell/hsc2hs.git\r\n\r\nExecutable hsc2hs\r\n    Default-Language: Haskell2010\r\n    Main-Is: Main.hs\r\n    Hs-Source-Dirs: src/\r\n    Other-Modules:\r\n        C\r\n        Common\r\n        CrossCodegen\r\n        DirectCodegen\r\n        Flags\r\n        HSCParser\r\n        ATTParser\r\n        UtilsCodegen\r\n        Compat.ResponseFile\r\n        Compat.TempFile\r\n        Paths_hsc2hs\r\n\r\n    c-sources:\r\n        cbits/utils.c\r\n\r\n    Other-Extensions: CPP, NoMonomorphismRestriction\r\n\r\n    Build-Depends: base       >= 4.3.0 && < 4.18,\r\n                   containers >= 0.4.0 && < 0.7,\r\n                   directory  >= 1.1.0 && < 1.4,\r\n                   filepath   >= 1.2.0 && < 1.5,\r\n                   process    >= 1.1.0 && < 1.7\r\n\r\n    if os(windows)\r\n      -- N.B. Job object support was irreparably broken prior to 1.6.8.\r\n      -- See https://github.com/haskell/process/issues/167.\r\n      Build-Depends: process  >= 1.6.8 && < 1.7\r\n\r\n    ghc-options:   -Wall\r\n    if flag(in-ghc-tree)\r\n       cpp-options: -DIN_GHC_TREE\r\n\r\ntest-suite spec\r\n  main-is:           Spec.hs\r\n  hs-source-dirs:    src/ test/\r\n  other-modules:     ATTParser Flags BDD\r\n  ghc-options:       -Wall -threaded\r\n  type:              exitcode-stdio-1.0\r\n  build-depends:     base,\r\n                     test-framework >=0.8.2.0 && <0.9,\r\n                     test-framework-hunit >=0.3.0.2 && <0.4,\r\n                     HUnit >=1.3.1.2 && <1.4 || >=1.6.0.0 && <1.7\r\n\r\n  default-language: Haskell2010\r\n";
    }