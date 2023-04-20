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
      specVersion = "1.2";
      identifier = { name = "executable-path"; version = "0.0.3.1"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "bkomuves (plus) hackage (at) gmail (dot) com";
      author = "Balazs Komuves";
      homepage = "http://code.haskell.org/~bkomuves/";
      url = "";
      synopsis = "Finding out the full path of the executable.";
      description = "The documentation of \"System.Environment.getProgName\" says that\n\\\"However, this is hard-to-impossible to implement on some non-Unix OSes,\nso instead, for maximum portability, we just return the leafname\nof the program as invoked.\\\"\nThis library tries to provide the missing path.\nNote: Since base 4.6.0.0, there is also a function\n\"System.Environment.getExecutablePath\".";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && true) (hsPkgs."directory" or (errorHandler.buildDepError "directory"))) ++ (pkgs.lib).optional (system.isLinux) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optionals (system.isFreebsd) [
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ]) ++ (pkgs.lib).optionals (system.isOpenbsd || system.isNetbsd) [
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ]) ++ (pkgs.lib).optional (system.isSolaris) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."kernel32" or (errorHandler.sysDepError "kernel32"));
        frameworks = (pkgs.lib).optional (system.isOsx) (pkgs."CoreFoundation" or (errorHandler.sysDepError "CoreFoundation"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/executable-path-0.0.3.1.tar.gz";
      sha256 = "9cc742b6d40a487b3af38dca6852ca3b50a0db94d42fe819576c84beb5adbc6f";
      });
    }) // {
    package-description-override = "Name:                executable-path\nVersion:             0.0.3.1\nSynopsis:            Finding out the full path of the executable.\n\nDescription:         The documentation of \"System.Environment.getProgName\" says that\n\n                     \\\"However, this is hard-to-impossible to implement on some non-Unix OSes, \n                     so instead, for maximum portability, we just return the leafname \n                     of the program as invoked.\\\"\n                     \n                     This library tries to provide the missing path.\n                     \n                     Note: Since base 4.6.0.0, there is also a function \n                     \"System.Environment.getExecutablePath\".\n\nLicense:             PublicDomain\nLicense-file:        LICENSE\nAuthor:              Balazs Komuves\nMaintainer:          bkomuves (plus) hackage (at) gmail (dot) com\nHomepage:            http://code.haskell.org/~bkomuves/\nStability:           Experimental\nCategory:            System\nTested-With:         GHC == 6.12.3 \nCabal-Version:       >= 1.2\nBuild-Type:          Simple\n\nLibrary\n  Build-Depends:       base >= 3 && < 5 , filepath \n  \n  if impl(ghc)\n    cpp-options:         -DWE_HAVE_GHC\n    build-depends:       directory\n    \n  Exposed-Modules:     System.Environment.Executable   \n  Extensions:          ForeignFunctionInterface, CPP, EmptyDataDecls\n  Hs-Source-Dirs:      .\n  \n  if os(darwin)\n    Frameworks:          CoreFoundation  \n    Other-Modules:       System.Environment.Executable.MacOSX\n   \n  if os(windows) \n    Extra-Libraries:     kernel32\n    Other-Modules:       System.Environment.Executable.Win32\n    \n  if os(linux) \n    Build-Depends:       unix\n    Other-Modules:       System.Environment.Executable.Linux\n    \n  if os(freebsd) \n    Build-Depends:       unix, directory\n    Other-Modules:       System.Environment.Executable.FreeBSD\n\n  if os(openbsd) || os(netbsd)\n    Build-Depends:       unix, directory\n    Other-Modules:       System.Environment.Executable.BSD\n\n  if os(solaris) \n    Build-Depends:       unix\n    Other-Modules:       System.Environment.Executable.Solaris\n    \n";
    }