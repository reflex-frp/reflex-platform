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
    flags = { devel = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "network"; version = "3.1.2.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto, Evan Borden";
      author = "";
      homepage = "https://github.com/haskell/network";
      url = "";
      synopsis = "Low-level networking interface";
      description = "This package provides a low-level networking interface.\n\n=== High-Level Packages\nOther packages provide higher level interfaces:\n\n* connection\n* hookup\n* network-simple\n\n=== Extended Packages\n@network@ seeks to provide a cross-platform core for networking. As such some\nAPIs live in extended libraries. Packages in the @network@ ecosystem are\noften prefixed with @network-@.\n\n==== @network-bsd@\nIn @network-3.0.0.0@ the @Network.BSD@ module was split off into its own\npackage, @network-bsd-3.0.0.0@.\n\n==== @network-uri@\nIn @network-2.6@ the @Network.URI@ module was split off into its own package,\n@network-uri-2.6@. If you're using the @Network.URI@ module you can\nautomatically get it from the right package by adding this to your @.cabal@\nfile:\n\n> library\n>   build-depends: network-uri-flag";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ];
        libs = (pkgs.lib).optionals (system.isSolaris) [
          (pkgs."nsl" or (errorHandler.sysDepError "nsl"))
          (pkgs."socket" or (errorHandler.sysDepError "socket"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (pkgs."ws2_32" or (errorHandler.sysDepError "ws2_32"))
          (pkgs."iphlpapi" or (errorHandler.sysDepError "iphlpapi"))
          (pkgs."mswsock" or (errorHandler.sysDepError "mswsock"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ];
          buildable = false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-3.1.2.7.tar.gz";
      sha256 = "7f7620fef1a1af3d3d6747f510e73223a5c600e7d7fd9ace073d1222bdc63d85";
      });
    }) // {
    package-description-override = "cabal-version:  1.18\nname:           network\nversion:        3.1.2.7\nlicense:        BSD3\nlicense-file:   LICENSE\nmaintainer:     Kazu Yamamoto, Evan Borden\nsynopsis:       Low-level networking interface\ndescription:\n  This package provides a low-level networking interface.\n  .\n  === High-Level Packages\n  Other packages provide higher level interfaces:\n  .\n  * connection\n  * hookup\n  * network-simple\n  .\n  === Extended Packages\n  @network@ seeks to provide a cross-platform core for networking. As such some\n  APIs live in extended libraries. Packages in the @network@ ecosystem are\n  often prefixed with @network-@.\n  .\n  ==== @network-bsd@\n  In @network-3.0.0.0@ the @Network.BSD@ module was split off into its own\n  package, @network-bsd-3.0.0.0@.\n  .\n  ==== @network-uri@\n  In @network-2.6@ the @Network.URI@ module was split off into its own package,\n  @network-uri-2.6@. If you're using the @Network.URI@ module you can\n  automatically get it from the right package by adding this to your @.cabal@\n  file:\n  .\n  > library\n  >   build-depends: network-uri-flag\ncategory:       Network\nbuild-type:     Configure\nextra-tmp-files:\n  config.log config.status autom4te.cache network.buildinfo\n  include/HsNetworkConfig.h\nextra-source-files:\n  README.md CHANGELOG.md\n  examples/*.hs tests/*.hs config.guess config.sub install-sh\n  configure.ac configure\n  include/HsNetworkConfig.h.in include/HsNet.h include/HsNetDef.h\n  -- C sources only used on some systems\n  cbits/asyncAccept.c cbits/initWinSock.c\n  cbits/winSockErr.c cbits/cmsg.c\nhomepage:       https://github.com/haskell/network\nbug-reports:    https://github.com/haskell/network/issues\ntested-with:   GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.3\n             , GHC == 8.10.1\n\nflag devel\n  description:          using tests for developers\n  default:              False\n\nlibrary\n  default-language: Haskell2010\n  exposed-modules:\n    Network.Socket\n    Network.Socket.Address\n    Network.Socket.ByteString\n    Network.Socket.ByteString.Lazy\n    Network.Socket.Internal\n  other-modules:\n    Network.Socket.Buffer\n    Network.Socket.ByteString.IO\n    Network.Socket.ByteString.Internal\n    Network.Socket.Cbits\n    Network.Socket.Fcntl\n    Network.Socket.Flag\n    Network.Socket.Handle\n    Network.Socket.If\n    Network.Socket.Imports\n    Network.Socket.Info\n    Network.Socket.Name\n    Network.Socket.Options\n    Network.Socket.ReadShow\n    Network.Socket.Shutdown\n    Network.Socket.SockAddr\n    Network.Socket.Syscall\n    Network.Socket.Types\n    Network.Socket.Unix\n\n  build-depends:\n    base >= 4.9 && < 5,\n    bytestring >= 0.10 && < 0.12,\n    deepseq,\n    directory\n\n  include-dirs: include\n  includes: HsNet.h HsNetDef.h alignment.h win32defs.h\n  install-includes: HsNet.h HsNetDef.h  alignment.h win32defs.h\n  c-sources: cbits/HsNet.c cbits/cmsg.c\n  ghc-options: -Wall -fwarn-tabs\n  build-tools: hsc2hs\n\n\n  -- Add some platform specific stuff\n  if !os(windows)\n    other-modules:\n      Network.Socket.ByteString.Lazy.Posix\n      Network.Socket.Posix.Cmsg\n      Network.Socket.Posix.CmsgHdr\n      Network.Socket.Posix.IOVec\n      Network.Socket.Posix.MsgHdr\n\n  if os(solaris)\n    extra-libraries: nsl, socket\n    cpp-options: -D__EXTENSIONS__ -D_XOPEN_SOURCE=500\n    cc-options: -D__EXTENSIONS__ -D_XOPEN_SOURCE=500\n\n  if os(windows)\n    other-modules:\n      Network.Socket.ByteString.Lazy.Windows\n      Network.Socket.Win32.Cmsg\n      Network.Socket.Win32.CmsgHdr\n      Network.Socket.Win32.WSABuf\n      Network.Socket.Win32.MsgHdr\n    c-sources: cbits/initWinSock.c, cbits/winSockErr.c, cbits/asyncAccept.c\n    extra-libraries: ws2_32, iphlpapi, mswsock\n    -- See https://github.com/haskell/network/pull/362\n    if impl(ghc >= 7.10)\n      cpp-options: -D_WIN32_WINNT=0x0600\n      cc-options: -D_WIN32_WINNT=0x0600\n\ntest-suite spec\n  default-language: Haskell2010\n  hs-source-dirs: tests\n  main-is: Spec.hs\n  if flag(devel)\n    cpp-options:  -DDEVELOPMENT\n  other-modules:\n    Network.Test.Common\n    Network.SocketSpec\n    Network.Socket.ByteStringSpec\n    Network.Socket.ByteString.LazySpec\n  type: exitcode-stdio-1.0\n  ghc-options: -Wall -threaded\n  -- NB: make sure to versions of hspec and hspec-discover\n  --     that work together; easiest way is to constraint\n  --     both packages to a small enough version range.\n  build-tools: hspec-discover >= 2.6\n  build-depends:\n    base >= 4.9 && < 5,\n    bytestring,\n    directory,\n    HUnit,\n    network,\n    temporary,\n    hspec >= 2.6,\n    QuickCheck\n\ntest-suite doctests\n  buildable: False\n  default-language: Haskell2010\n  hs-source-dirs: tests\n  main-is: doctests.hs\n  type: exitcode-stdio-1.0\n\n  build-depends:\n    base >= 4.9 && < 5,\n    doctest >= 0.10.1,\n    network\n\n  ghc-options: -Wall\n\nsource-repository head\n  type:     git\n  location: git://github.com/haskell/network.git\n";
    }