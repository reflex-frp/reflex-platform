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
      identifier = { name = "network"; version = "3.1.2.1"; };
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
      url = "http://hackage.haskell.org/package/network-3.1.2.1.tar.gz";
      sha256 = "fcaa954445cb575ff04d088e719452e356324b6acb98c5aefd2541a069439d4a";
      });
    }) // {
    package-description-override = "cabal-version:  1.18\r\nname:           network\r\nversion:        3.1.2.1\r\nx-revision: 1\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nmaintainer:     Kazu Yamamoto, Evan Borden\r\nsynopsis:       Low-level networking interface\r\ndescription:\r\n  This package provides a low-level networking interface.\r\n  .\r\n  === High-Level Packages\r\n  Other packages provide higher level interfaces:\r\n  .\r\n  * connection\r\n  * hookup\r\n  * network-simple\r\n  .\r\n  === Extended Packages\r\n  @network@ seeks to provide a cross-platform core for networking. As such some\r\n  APIs live in extended libraries. Packages in the @network@ ecosystem are\r\n  often prefixed with @network-@.\r\n  .\r\n  ==== @network-bsd@\r\n  In @network-3.0.0.0@ the @Network.BSD@ module was split off into its own\r\n  package, @network-bsd-3.0.0.0@.\r\n  .\r\n  ==== @network-uri@\r\n  In @network-2.6@ the @Network.URI@ module was split off into its own package,\r\n  @network-uri-2.6@. If you're using the @Network.URI@ module you can\r\n  automatically get it from the right package by adding this to your @.cabal@\r\n  file:\r\n  .\r\n  > library\r\n  >   build-depends: network-uri-flag\r\ncategory:       Network\r\nbuild-type:     Configure\r\nextra-tmp-files:\r\n  config.log config.status autom4te.cache network.buildinfo\r\n  include/HsNetworkConfig.h\r\nextra-source-files:\r\n  README.md CHANGELOG.md\r\n  examples/*.hs tests/*.hs config.guess config.sub install-sh\r\n  configure.ac configure\r\n  include/HsNetworkConfig.h.in include/HsNet.h include/HsNetDef.h\r\n  -- C sources only used on some systems\r\n  cbits/asyncAccept.c cbits/initWinSock.c\r\n  cbits/winSockErr.c cbits/cmsg.c\r\nhomepage:       https://github.com/haskell/network\r\nbug-reports:    https://github.com/haskell/network/issues\r\ntested-with:   GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.3\r\n             , GHC == 8.10.1\r\n\r\nflag devel\r\n  description:          using tests for developers\r\n  default:              False\r\n\r\nlibrary\r\n  default-language: Haskell2010\r\n  exposed-modules:\r\n    Network.Socket\r\n    Network.Socket.Address\r\n    Network.Socket.ByteString\r\n    Network.Socket.ByteString.Lazy\r\n    Network.Socket.Internal\r\n  other-modules:\r\n    Network.Socket.Buffer\r\n    Network.Socket.ByteString.IO\r\n    Network.Socket.ByteString.Internal\r\n    Network.Socket.Cbits\r\n    Network.Socket.Fcntl\r\n    Network.Socket.Flag\r\n    Network.Socket.Handle\r\n    Network.Socket.If\r\n    Network.Socket.Imports\r\n    Network.Socket.Info\r\n    Network.Socket.Name\r\n    Network.Socket.Options\r\n    Network.Socket.ReadShow\r\n    Network.Socket.Shutdown\r\n    Network.Socket.SockAddr\r\n    Network.Socket.Syscall\r\n    Network.Socket.Types\r\n    Network.Socket.Unix\r\n\r\n  build-depends:\r\n    base >= 4.9 && < 5,\r\n    bytestring >= 0.10 && < 0.12,\r\n    deepseq,\r\n    directory\r\n\r\n  include-dirs: include\r\n  includes: HsNet.h HsNetDef.h alignment.h win32defs.h\r\n  install-includes: HsNet.h HsNetDef.h  alignment.h win32defs.h\r\n  c-sources: cbits/HsNet.c cbits/cmsg.c\r\n  ghc-options: -Wall -fwarn-tabs\r\n  build-tools: hsc2hs\r\n\r\n\r\n  -- Add some platform specific stuff\r\n  if !os(windows)\r\n    other-modules:\r\n      Network.Socket.ByteString.Lazy.Posix\r\n      Network.Socket.Posix.Cmsg\r\n      Network.Socket.Posix.CmsgHdr\r\n      Network.Socket.Posix.IOVec\r\n      Network.Socket.Posix.MsgHdr\r\n\r\n  if os(solaris)\r\n    extra-libraries: nsl, socket\r\n\r\n  if os(windows)\r\n    other-modules:\r\n      Network.Socket.ByteString.Lazy.Windows\r\n      Network.Socket.Win32.Cmsg\r\n      Network.Socket.Win32.CmsgHdr\r\n      Network.Socket.Win32.WSABuf\r\n      Network.Socket.Win32.MsgHdr\r\n    c-sources: cbits/initWinSock.c, cbits/winSockErr.c, cbits/asyncAccept.c\r\n    extra-libraries: ws2_32, iphlpapi, mswsock\r\n    -- See https://github.com/haskell/network/pull/362\r\n    if impl(ghc >= 7.10)\r\n      cpp-options: -D_WIN32_WINNT=0x0600\r\n      cc-options: -D_WIN32_WINNT=0x0600\r\n\r\ntest-suite spec\r\n  default-language: Haskell2010\r\n  hs-source-dirs: tests\r\n  main-is: Spec.hs\r\n  if flag(devel)\r\n    cpp-options:  -DDEVELOPMENT\r\n  other-modules:\r\n    Network.Test.Common\r\n    Network.SocketSpec\r\n    Network.Socket.ByteStringSpec\r\n    Network.Socket.ByteString.LazySpec\r\n  type: exitcode-stdio-1.0\r\n  ghc-options: -Wall -threaded\r\n  -- NB: make sure to versions of hspec and hspec-discover\r\n  --     that work together; easiest way is to constraint\r\n  --     both packages to a small enough version range.\r\n  build-tools: hspec-discover >= 2.6\r\n  build-depends:\r\n    base >= 4.9 && < 5,\r\n    bytestring,\r\n    directory,\r\n    HUnit,\r\n    network,\r\n    temporary,\r\n    hspec >= 2.6,\r\n    QuickCheck\r\n\r\ntest-suite doctests\r\n  buildable: False\r\n  default-language: Haskell2010\r\n  hs-source-dirs: tests\r\n  main-is: doctests.hs\r\n  type: exitcode-stdio-1.0\r\n\r\n  build-depends:\r\n    base >= 4.9 && < 5,\r\n    doctest >= 0.10.1,\r\n    network\r\n\r\n  ghc-options: -Wall\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/haskell/network.git\r\n";
    }