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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "Win32"; version = "2.6.2.1"; };
      license = "BSD-3-Clause";
      copyright = "Alastair Reid, 1999-2003; shelarcy, 2012-2013; Tamar Christina, 2016-2018";
      maintainer = "Haskell Libraries <libraries@haskell.org>";
      author = "Alastair Reid, shelarcy, Tamar Christina";
      homepage = "https://github.com/haskell/win32";
      url = "";
      synopsis = "A binding to Windows Win32 API.";
      description = "This library contains direct bindings to the Windows Win32 APIs for Haskell.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unbuildable" or (errorHandler.buildDepError "unbuildable"));
        libs = [
          (pkgs."user32" or (errorHandler.sysDepError "user32"))
          (pkgs."gdi32" or (errorHandler.sysDepError "gdi32"))
          (pkgs."winmm" or (errorHandler.sysDepError "winmm"))
          (pkgs."advapi32" or (errorHandler.sysDepError "advapi32"))
          (pkgs."shell32" or (errorHandler.sysDepError "shell32"))
          (pkgs."shfolder" or (errorHandler.sysDepError "shfolder"))
          (pkgs."shlwapi" or (errorHandler.sysDepError "shlwapi"))
          (pkgs."msimg32" or (errorHandler.sysDepError "msimg32"))
          (pkgs."imm32" or (errorHandler.sysDepError "imm32"))
          ];
        buildable = if !system.isWindows then false else true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }
