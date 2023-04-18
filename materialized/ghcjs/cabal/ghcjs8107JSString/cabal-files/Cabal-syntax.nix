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
      specVersion = "1.22";
      identifier = { name = "Cabal-syntax"; version = "3.8.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2003-2022, Cabal Development Team (see AUTHORS file)";
      maintainer = "cabal-devel@haskell.org";
      author = "Cabal Development Team <cabal-devel@haskell.org>";
      homepage = "http://www.haskell.org/cabal/";
      url = "";
      synopsis = "A library for working with .cabal files";
      description = "This library provides tools for reading and manipulating the .cabal file\nformat.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Cabal-syntax-3.8.1.0.tar.gz";
      sha256 = "07e8ddb19fe01781485f1522b6afc22aba680b0ab28ebe6bbfb84a2dd698ce0f";
      });
    }) // {
    package-description-override = "cabal-version: 1.22\r\nname:          Cabal-syntax\r\nversion:       3.8.1.0\r\nx-revision: 1\r\ncopyright:     2003-2022, Cabal Development Team (see AUTHORS file)\r\nlicense:       BSD3\r\nlicense-file:  LICENSE\r\nauthor:        Cabal Development Team <cabal-devel@haskell.org>\r\nmaintainer:    cabal-devel@haskell.org\r\nhomepage:      http://www.haskell.org/cabal/\r\nbug-reports:   https://github.com/haskell/cabal/issues\r\nsynopsis:      A library for working with .cabal files\r\ndescription:\r\n    This library provides tools for reading and manipulating the .cabal file\r\n    format.\r\ncategory:       Distribution\r\nbuild-type:     Simple\r\n\r\nextra-source-files:\r\n  README.md ChangeLog.md\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/cabal/\r\n  subdir:   Cabal-syntax\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  hs-source-dirs: src\r\n\r\n  build-depends:\r\n    array      >= 0.4.0.1  && < 0.6,\r\n    base       >= 4.9      && < 5,\r\n    binary     >= 0.7      && < 0.9,\r\n    bytestring >= 0.10.0.0 && < 0.12,\r\n    containers >= 0.5.0.0  && < 0.7,\r\n    deepseq    >= 1.3.0.1  && < 1.5,\r\n    directory  >= 1.2      && < 1.4,\r\n    filepath   >= 1.3.0.1  && < 1.5,\r\n    mtl        >= 2.1      && < 2.3,\r\n    parsec     >= 3.1.13.0 && < 3.2,\r\n    pretty     >= 1.1.1    && < 1.2,\r\n    text       (>= 1.2.3.0 && < 1.3) || (>= 2.0 && < 2.1),\r\n    time       >= 1.4.0.1  && < 1.13,\r\n    -- transformers-0.4.0.0 doesn't have record syntax e.g. for Identity\r\n    -- See also https://github.com/ekmett/transformers-compat/issues/35\r\n    transformers (>= 0.3      && < 0.4) || (>=0.4.1.0 && <0.6)\r\n\r\n  if os(windows)\r\n    build-depends: Win32 >= 2.3.0.0 && < 2.14\r\n  else\r\n    build-depends: unix  >= 2.6.0.0 && < 2.8\r\n\r\n  ghc-options: -Wall -fno-ignore-asserts -fwarn-tabs -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates\r\n  ghc-options: -Wcompat -Wnoncanonical-monad-instances\r\n\r\n  if impl(ghc <8.8)\r\n    ghc-options: -Wnoncanonical-monadfail-instances\r\n\r\n  exposed-modules:\r\n    Distribution.Backpack\r\n    Distribution.CabalSpecVersion\r\n    Distribution.Compat.Binary\r\n    Distribution.Compat.CharParsing\r\n    Distribution.Compat.DList\r\n    Distribution.Compat.Exception\r\n    Distribution.Compat.Graph\r\n    Distribution.Compat.Lens\r\n    Distribution.Compat.MonadFail\r\n    Distribution.Compat.Newtype\r\n    Distribution.Compat.NonEmptySet\r\n    Distribution.Compat.Parsing\r\n    Distribution.Compat.Prelude\r\n    Distribution.Compat.Semigroup\r\n    Distribution.Compat.Typeable\r\n    Distribution.Compiler\r\n    Distribution.FieldGrammar\r\n    Distribution.FieldGrammar.Class\r\n    Distribution.FieldGrammar.FieldDescrs\r\n    Distribution.FieldGrammar.Newtypes\r\n    Distribution.FieldGrammar.Parsec\r\n    Distribution.FieldGrammar.Pretty\r\n    Distribution.Fields\r\n    Distribution.Fields.ConfVar\r\n    Distribution.Fields.Field\r\n    Distribution.Fields.Lexer\r\n    Distribution.Fields.LexerMonad\r\n    Distribution.Fields.ParseResult\r\n    Distribution.Fields.Parser\r\n    Distribution.Fields.Pretty\r\n    Distribution.InstalledPackageInfo\r\n    Distribution.License\r\n    Distribution.ModuleName\r\n    Distribution.Package\r\n    Distribution.PackageDescription\r\n    Distribution.PackageDescription.Configuration\r\n    Distribution.PackageDescription.FieldGrammar\r\n    Distribution.PackageDescription.Parsec\r\n    Distribution.PackageDescription.PrettyPrint\r\n    Distribution.PackageDescription.Quirks\r\n    Distribution.PackageDescription.Utils\r\n    Distribution.Parsec\r\n    Distribution.Parsec.Error\r\n    Distribution.Parsec.FieldLineStream\r\n    Distribution.Parsec.Position\r\n    Distribution.Parsec.Warning\r\n    Distribution.Pretty\r\n    Distribution.SPDX\r\n    Distribution.SPDX.License\r\n    Distribution.SPDX.LicenseExceptionId\r\n    Distribution.SPDX.LicenseExpression\r\n    Distribution.SPDX.LicenseId\r\n    Distribution.SPDX.LicenseListVersion\r\n    Distribution.SPDX.LicenseReference\r\n    Distribution.System\r\n    Distribution.Text\r\n    Distribution.Types.AbiDependency\r\n    Distribution.Types.AbiHash\r\n    Distribution.Types.Benchmark\r\n    Distribution.Types.Benchmark.Lens\r\n    Distribution.Types.BenchmarkInterface\r\n    Distribution.Types.BenchmarkType\r\n    Distribution.Types.BuildInfo\r\n    Distribution.Types.BuildInfo.Lens\r\n    Distribution.Types.BuildType\r\n    Distribution.Types.Component\r\n    Distribution.Types.ComponentId\r\n    Distribution.Types.ComponentName\r\n    Distribution.Types.ComponentRequestedSpec\r\n    Distribution.Types.CondTree\r\n    Distribution.Types.Condition\r\n    Distribution.Types.ConfVar\r\n    Distribution.Types.Dependency\r\n    Distribution.Types.DependencyMap\r\n    Distribution.Types.ExeDependency\r\n    Distribution.Types.Executable\r\n    Distribution.Types.Executable.Lens\r\n    Distribution.Types.ExecutableScope\r\n    Distribution.Types.ExposedModule\r\n    Distribution.Types.Flag\r\n    Distribution.Types.ForeignLib\r\n    Distribution.Types.ForeignLib.Lens\r\n    Distribution.Types.ForeignLibOption\r\n    Distribution.Types.ForeignLibType\r\n    Distribution.Types.GenericPackageDescription\r\n    Distribution.Types.GenericPackageDescription.Lens\r\n    Distribution.Types.HookedBuildInfo\r\n    Distribution.Types.IncludeRenaming\r\n    Distribution.Types.InstalledPackageInfo\r\n    Distribution.Types.InstalledPackageInfo.Lens\r\n    Distribution.Types.InstalledPackageInfo.FieldGrammar\r\n    Distribution.Types.LegacyExeDependency\r\n    Distribution.Types.Lens\r\n    Distribution.Types.Library\r\n    Distribution.Types.Library.Lens\r\n    Distribution.Types.LibraryName\r\n    Distribution.Types.LibraryVisibility\r\n    Distribution.Types.Mixin\r\n    Distribution.Types.Module\r\n    Distribution.Types.ModuleReexport\r\n    Distribution.Types.ModuleRenaming\r\n    Distribution.Types.MungedPackageId\r\n    Distribution.Types.MungedPackageName\r\n    Distribution.Types.PackageDescription\r\n    Distribution.Types.PackageDescription.Lens\r\n    Distribution.Types.PackageId\r\n    Distribution.Types.PackageId.Lens\r\n    Distribution.Types.PackageName\r\n    Distribution.Types.PackageVersionConstraint\r\n    Distribution.Types.PkgconfigDependency\r\n    Distribution.Types.PkgconfigName\r\n    Distribution.Types.PkgconfigVersion\r\n    Distribution.Types.PkgconfigVersionRange\r\n    Distribution.Types.SetupBuildInfo\r\n    Distribution.Types.SetupBuildInfo.Lens\r\n    Distribution.Types.SourceRepo\r\n    Distribution.Types.SourceRepo.Lens\r\n    Distribution.Types.TestSuite\r\n    Distribution.Types.TestSuite.Lens\r\n    Distribution.Types.TestSuiteInterface\r\n    Distribution.Types.TestType\r\n    Distribution.Types.UnitId\r\n    Distribution.Types.UnqualComponentName\r\n    Distribution.Types.Version\r\n    Distribution.Types.VersionInterval\r\n    Distribution.Types.VersionInterval.Legacy\r\n    Distribution.Types.VersionRange\r\n    Distribution.Types.VersionRange.Internal\r\n    Distribution.Utils.Base62\r\n    Distribution.Utils.Generic\r\n    Distribution.Utils.MD5\r\n    Distribution.Utils.Path\r\n    Distribution.Utils.ShortText\r\n    Distribution.Utils.String\r\n    Distribution.Utils.Structured\r\n    Distribution.Version\r\n    Language.Haskell.Extension\r\n\r\n  other-extensions:\r\n    BangPatterns\r\n    CPP\r\n    DefaultSignatures\r\n    DeriveDataTypeable\r\n    DeriveFoldable\r\n    DeriveFunctor\r\n    DeriveGeneric\r\n    DeriveTraversable\r\n    ExistentialQuantification\r\n    FlexibleContexts\r\n    FlexibleInstances\r\n    GeneralizedNewtypeDeriving\r\n    ImplicitParams\r\n    KindSignatures\r\n    NondecreasingIndentation\r\n    OverloadedStrings\r\n    PatternSynonyms\r\n    RankNTypes\r\n    RecordWildCards\r\n    ScopedTypeVariables\r\n    StandaloneDeriving\r\n    Trustworthy\r\n    TypeFamilies\r\n    TypeOperators\r\n    TypeSynonymInstances\r\n    UndecidableInstances\r\n";
    }