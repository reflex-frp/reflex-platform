{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { native-dns = true; lukko = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cabal-install"; version = "3.8.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2003-2022, Cabal Development Team";
      maintainer = "Cabal Development Team <cabal-devel@haskell.org>";
      author = "Cabal Development Team (see AUTHORS file)";
      homepage = "http://www.haskell.org/cabal/";
      url = "";
      synopsis = "The command-line interface for Cabal and Hackage.";
      description = "The \\'cabal\\' command-line program simplifies the process of managing\nHaskell software by automating the fetching, configuration, compilation\nand installation of Haskell libraries and programs.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "bash-completion/cabal" "changelog" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
          (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."echo" or (errorHandler.buildDepError "echo"))
          (hsPkgs."edit-distance" or (errorHandler.buildDepError "edit-distance"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."hackage-security" or (errorHandler.buildDepError "hackage-security"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
          (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          ] ++ (pkgs.lib).optionals (flags.native-dns) (if system.isWindows
          then [ (hsPkgs."windns" or (errorHandler.buildDepError "windns")) ]
          else [
            (hsPkgs."resolv" or (errorHandler.buildDepError "resolv"))
            ])) ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ])) ++ (pkgs.lib).optional (flags.lukko) (hsPkgs."lukko" or (errorHandler.buildDepError "lukko"));
        buildable = true;
        modules = [
          "Distribution/Deprecated/ParseUtils"
          "Distribution/Deprecated/ReadP"
          "Distribution/Deprecated/ViewAsFieldDescr"
          "Distribution/Client/BuildReports/Anonymous"
          "Distribution/Client/BuildReports/Lens"
          "Distribution/Client/BuildReports/Storage"
          "Distribution/Client/BuildReports/Types"
          "Distribution/Client/BuildReports/Upload"
          "Distribution/Client/Check"
          "Distribution/Client/CmdBench"
          "Distribution/Client/CmdBuild"
          "Distribution/Client/CmdClean"
          "Distribution/Client/CmdConfigure"
          "Distribution/Client/CmdErrorMessages"
          "Distribution/Client/CmdExec"
          "Distribution/Client/CmdFreeze"
          "Distribution/Client/CmdHaddock"
          "Distribution/Client/CmdInstall"
          "Distribution/Client/CmdInstall/ClientInstallFlags"
          "Distribution/Client/CmdInstall/ClientInstallTargetSelector"
          "Distribution/Client/CmdLegacy"
          "Distribution/Client/CmdListBin"
          "Distribution/Client/CmdOutdated"
          "Distribution/Client/CmdRepl"
          "Distribution/Client/CmdRun"
          "Distribution/Client/CmdSdist"
          "Distribution/Client/CmdTest"
          "Distribution/Client/CmdUpdate"
          "Distribution/Client/Compat/Directory"
          "Distribution/Client/Compat/ExecutablePath"
          "Distribution/Client/Compat/Orphans"
          "Distribution/Client/Compat/Prelude"
          "Distribution/Client/Compat/Process"
          "Distribution/Client/Compat/Semaphore"
          "Distribution/Client/Config"
          "Distribution/Client/Configure"
          "Distribution/Client/Dependency"
          "Distribution/Client/Dependency/Types"
          "Distribution/Client/DistDirLayout"
          "Distribution/Client/Fetch"
          "Distribution/Client/FetchUtils"
          "Distribution/Client/FileMonitor"
          "Distribution/Client/Freeze"
          "Distribution/Client/GZipUtils"
          "Distribution/Client/GenBounds"
          "Distribution/Client/Get"
          "Distribution/Client/Glob"
          "Distribution/Client/GlobalFlags"
          "Distribution/Client/Haddock"
          "Distribution/Client/HashValue"
          "Distribution/Client/HttpUtils"
          "Distribution/Client/IndexUtils"
          "Distribution/Client/IndexUtils/ActiveRepos"
          "Distribution/Client/IndexUtils/IndexState"
          "Distribution/Client/IndexUtils/Timestamp"
          "Distribution/Client/Init"
          "Distribution/Client/Init/Defaults"
          "Distribution/Client/Init/FileCreators"
          "Distribution/Client/Init/FlagExtractors"
          "Distribution/Client/Init/Format"
          "Distribution/Client/Init/Interactive/Command"
          "Distribution/Client/Init/NonInteractive/Command"
          "Distribution/Client/Init/NonInteractive/Heuristics"
          "Distribution/Client/Init/Licenses"
          "Distribution/Client/Init/Prompt"
          "Distribution/Client/Init/Simple"
          "Distribution/Client/Init/Types"
          "Distribution/Client/Init/Utils"
          "Distribution/Client/Install"
          "Distribution/Client/InstallPlan"
          "Distribution/Client/InstallSymlink"
          "Distribution/Client/JobControl"
          "Distribution/Client/List"
          "Distribution/Client/Manpage"
          "Distribution/Client/ManpageFlags"
          "Distribution/Client/Nix"
          "Distribution/Client/NixStyleOptions"
          "Distribution/Client/PackageHash"
          "Distribution/Client/ParseUtils"
          "Distribution/Client/ProjectBuilding"
          "Distribution/Client/ProjectBuilding/Types"
          "Distribution/Client/ProjectConfig"
          "Distribution/Client/ProjectConfig/Legacy"
          "Distribution/Client/ProjectConfig/Types"
          "Distribution/Client/ProjectFlags"
          "Distribution/Client/ProjectOrchestration"
          "Distribution/Client/ProjectPlanOutput"
          "Distribution/Client/ProjectPlanning"
          "Distribution/Client/ProjectPlanning/Types"
          "Distribution/Client/RebuildMonad"
          "Distribution/Client/Reconfigure"
          "Distribution/Client/Run"
          "Distribution/Client/Sandbox"
          "Distribution/Client/Sandbox/PackageEnvironment"
          "Distribution/Client/SavedFlags"
          "Distribution/Client/ScriptUtils"
          "Distribution/Client/Security/DNS"
          "Distribution/Client/Security/HTTP"
          "Distribution/Client/Setup"
          "Distribution/Client/SetupWrapper"
          "Distribution/Client/SolverInstallPlan"
          "Distribution/Client/SourceFiles"
          "Distribution/Client/SrcDist"
          "Distribution/Client/Store"
          "Distribution/Client/Tar"
          "Distribution/Client/TargetProblem"
          "Distribution/Client/TargetSelector"
          "Distribution/Client/Targets"
          "Distribution/Client/Types"
          "Distribution/Client/Types/AllowNewer"
          "Distribution/Client/Types/BuildResults"
          "Distribution/Client/Types/ConfiguredId"
          "Distribution/Client/Types/ConfiguredPackage"
          "Distribution/Client/Types/Credentials"
          "Distribution/Client/Types/InstallMethod"
          "Distribution/Client/Types/OverwritePolicy"
          "Distribution/Client/Types/PackageLocation"
          "Distribution/Client/Types/PackageSpecifier"
          "Distribution/Client/Types/ReadyPackage"
          "Distribution/Client/Types/Repo"
          "Distribution/Client/Types/RepoName"
          "Distribution/Client/Types/SourcePackageDb"
          "Distribution/Client/Types/SourceRepo"
          "Distribution/Client/Types/WriteGhcEnvironmentFilesPolicy"
          "Distribution/Client/Upload"
          "Distribution/Client/Utils"
          "Distribution/Client/Utils/Json"
          "Distribution/Client/Utils/Parsec"
          "Distribution/Client/VCS"
          "Distribution/Client/Version"
          "Distribution/Client/Win32SelfUpgrade"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "cabal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          libs = (pkgs.lib).optional (system.isAix) (pkgs."bsd" or (errorHandler.sysDepError "bsd"));
          buildable = true;
          hsSourceDirs = [ "main" ];
          mainPath = (([
            "Main.hs"
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.8") "") ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.10") "") ++ (pkgs.lib).optional (system.isAix) "";
          };
        };
      tests = {
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."Cabal-tree-diff" or (errorHandler.buildDepError "Cabal-tree-diff"))
            (hsPkgs."Cabal-QuickCheck" or (errorHandler.buildDepError "Cabal-QuickCheck"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          modules = [
            "UnitTests/Distribution/Client/ArbitraryInstances"
            "UnitTests/Distribution/Client/BuildReport"
            "UnitTests/Distribution/Client/Configure"
            "UnitTests/Distribution/Client/FetchUtils"
            "UnitTests/Distribution/Client/Get"
            "UnitTests/Distribution/Client/Glob"
            "UnitTests/Distribution/Client/GZipUtils"
            "UnitTests/Distribution/Client/IndexUtils"
            "UnitTests/Distribution/Client/IndexUtils/Timestamp"
            "UnitTests/Distribution/Client/Init"
            "UnitTests/Distribution/Client/Init/Golden"
            "UnitTests/Distribution/Client/Init/Interactive"
            "UnitTests/Distribution/Client/Init/NonInteractive"
            "UnitTests/Distribution/Client/Init/Simple"
            "UnitTests/Distribution/Client/Init/Utils"
            "UnitTests/Distribution/Client/Init/FileCreators"
            "UnitTests/Distribution/Client/InstallPlan"
            "UnitTests/Distribution/Client/JobControl"
            "UnitTests/Distribution/Client/ProjectConfig"
            "UnitTests/Distribution/Client/ProjectPlanning"
            "UnitTests/Distribution/Client/Store"
            "UnitTests/Distribution/Client/Tar"
            "UnitTests/Distribution/Client/Targets"
            "UnitTests/Distribution/Client/TreeDiffInstances"
            "UnitTests/Distribution/Client/UserConfig"
            "UnitTests/Distribution/Solver/Modular/Builder"
            "UnitTests/Distribution/Solver/Modular/RetryLog"
            "UnitTests/Distribution/Solver/Modular/Solver"
            "UnitTests/Distribution/Solver/Modular/DSL"
            "UnitTests/Distribution/Solver/Modular/DSL/TestCaseUtils"
            "UnitTests/Distribution/Solver/Modular/WeightedPSQ"
            "UnitTests/Distribution/Solver/Types/OptionalStanza"
            "UnitTests/Options"
            "UnitTests/TempTestDir"
            ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "UnitTests.hs" ];
          };
        "mem-use-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          modules = [
            "UnitTests/Distribution/Solver/Modular/DSL"
            "UnitTests/Distribution/Solver/Modular/DSL/TestCaseUtils"
            "UnitTests/Distribution/Solver/Modular/MemoryUsage"
            "UnitTests/Options"
            ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "MemoryUsageTests.hs" ];
          };
        "integration-tests2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            ];
          buildable = true;
          hsSourceDirs = [ "tests" ];
          mainPath = [ "IntegrationTests2.hs" ];
          };
        "long-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."Cabal-QuickCheck" or (errorHandler.buildDepError "Cabal-QuickCheck"))
            (hsPkgs."Cabal-described" or (errorHandler.buildDepError "Cabal-described"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            ];
          buildable = true;
          modules = [
            "UnitTests/Distribution/Client/ArbitraryInstances"
            "UnitTests/Distribution/Client/Described"
            "UnitTests/Distribution/Client/DescribedInstances"
            "UnitTests/Distribution/Client/FileMonitor"
            "UnitTests/Distribution/Client/VCS"
            "UnitTests/Distribution/Solver/Modular/DSL"
            "UnitTests/Distribution/Solver/Modular/QuickCheck"
            "UnitTests/Distribution/Solver/Modular/QuickCheck/Utils"
            "UnitTests/Options"
            "UnitTests/TempTestDir"
            ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "LongTests.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }