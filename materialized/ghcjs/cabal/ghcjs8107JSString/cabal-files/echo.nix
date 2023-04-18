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
      identifier = { name = "echo"; version = "0.1.4"; };
      license = "BSD-3-Clause";
      copyright = "(C) 2016-2017 Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Ryan Scott";
      homepage = "https://github.com/RyanGlScott/echo";
      url = "";
      synopsis = "A cross-platform, cross-console way to handle echoing terminal input";
      description = "The @base@ library exposes the @hGetEcho@ and @hSetEcho@ functions\nfor querying and setting echo status, but unfortunately, neither\nfunction works with MinTTY consoles on Windows. This is a serious\nissue, since @hGetEcho@ and @hSetEcho@ are often used to disable\ninput echoing when a program prompts for a password, so many\nprograms will reveal your password as you type it on MinTTY!\n\nThis library provides an alternative interface which works\nwith both MinTTY and other consoles. An example is included\nwhich demonstrates how one might prompt for a password using\nthis library. To build it, make sure to configure with the\n@-fexample@ flag.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."mintty" or (errorHandler.buildDepError "mintty"))
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          ];
        buildable = true;
        };
      exes = {
        "password" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."echo" or (errorHandler.buildDepError "echo"))
            ];
          buildable = if !flags.example then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/echo-0.1.4.tar.gz";
      sha256 = "c9fe1bf2904825a65b667251ec644f197b71dc5c209d2d254be5de3d496b0e43";
      });
    }) // {
    package-description-override = "name:                echo\nversion:             0.1.4\nsynopsis:            A cross-platform, cross-console way to handle echoing terminal input\ndescription:         The @base@ library exposes the @hGetEcho@ and @hSetEcho@ functions\n                     for querying and setting echo status, but unfortunately, neither\n                     function works with MinTTY consoles on Windows. This is a serious\n                     issue, since @hGetEcho@ and @hSetEcho@ are often used to disable\n                     input echoing when a program prompts for a password, so many\n                     programs will reveal your password as you type it on MinTTY!\n                     .\n                     This library provides an alternative interface which works\n                     with both MinTTY and other consoles. An example is included\n                     which demonstrates how one might prompt for a password using\n                     this library. To build it, make sure to configure with the\n                     @-fexample@ flag.\nhomepage:            https://github.com/RyanGlScott/echo\nbug-reports:         https://github.com/RyanGlScott/echo/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Ryan Scott\nmaintainer:          Ryan Scott <ryan.gl.scott@gmail.com>\nstability:           Provisional\ncopyright:           (C) 2016-2017 Ryan Scott\ncategory:            System\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md, README.md\ncabal-version:       >=1.10\ntested-with:         GHC == 7.0.4\n                   , GHC == 7.2.2\n                   , GHC == 7.4.2\n                   , GHC == 7.6.3\n                   , GHC == 7.8.4\n                   , GHC == 7.10.3\n                   , GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.2\n\nsource-repository head\n  type:                git\n  location:            https://github.com/RyanGlScott/echo\n\nflag example\n  description:         Build the bundled example program.\n  default:             False\n\nlibrary\n  exposed-modules:     System.IO.Echo\n                       System.IO.Echo.Internal\n\n  build-depends:       base    >= 4.3     && < 5\n                     , process >= 1.0.1.1 && < 1.7\n  if os(windows)\n    cpp-options:       \"-DWINDOWS\"\n    build-depends:     mintty >= 0.1 && < 0.2\n                     , Win32  >= 2   && < 3\n\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n\nexecutable password\n  if !flag(example)\n    buildable:         False\n\n  main-is:             Password.hs\n  build-depends:       base >= 4.3 && < 5\n                     , echo\n  hs-source-dirs:      example\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n";
    }