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
      identifier = { name = "hostname"; version = "1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Max Bolingbroke <batterseapower@hotmail.com>";
      author = "Max Bolingbroke <batterseapower@hotmail.com>";
      homepage = "";
      url = "";
      synopsis = "A very simple package providing a cross-platform means of determining the hostname";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."kernel32" or (errorHandler.sysDepError "kernel32"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hostname-1.0.tar.gz";
      sha256 = "9b43dab1b6da521f35685b20555da00738c8e136eb972458c786242406a9cf5c";
      });
    }) // {
    package-description-override = "Name:               hostname\nVersion:            1.0\nCabal-Version:      >= 1.2\nCategory:           Network\nSynopsis:           A very simple package providing a cross-platform means of determining the hostname\nLicense:            BSD3\nLicense-File:       LICENSE\nAuthor:             Max Bolingbroke <batterseapower@hotmail.com>\nMaintainer:         Max Bolingbroke <batterseapower@hotmail.com>\nBuild-Type:         Simple\n\nLibrary\n        Exposed-Modules: Network.HostName\n                         \n        Build-Depends:   base >= 3 && < 5\n                         \n        Extensions:      CPP, ForeignFunctionInterface\n        \n        if os(windows)\n                         Build-Depends:   Win32 >= 2.0\n                         Cpp-Options:     -DWINDOWS\n                         Extra-Libraries: \"kernel32\"\n";
    }