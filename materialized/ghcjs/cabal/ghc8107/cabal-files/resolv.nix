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
      specVersion = "2.2";
      identifier = { name = "resolv"; version = "0.1.2.0"; };
      license = "GPL-2.0-or-later";
      copyright = "";
      maintainer = "hvr@gnu.org";
      author = "Herbert Valerio Riedel";
      homepage = "";
      url = "";
      synopsis = "Domain Name Service (DNS) lookup via the libresolv standard library routines";
      description = "This package implements an API for accessing\nthe [Domain Name Service (DNS)](https://tools.ietf.org/html/rfc1035)\nresolver service via the standard @libresolv@ system library (whose\nAPI is often available directly via the standard @libc@ C library) on\nUnix systems.\n\nThis package also includes support for decoding message record types\nas defined in the following RFCs:\n\n- [RFC 1035](https://tools.ietf.org/html/rfc1035): Domain Names - Implementation And Specification\n- [RFC 1183](https://tools.ietf.org/html/rfc1183): New DNS RR Definitions\n- [RFC 2782](https://tools.ietf.org/html/rfc2782): A DNS RR for specifying the location of services (DNS SRV)\n- [RFC 2915](https://tools.ietf.org/html/rfc2915): The Naming Authority Pointer (NAPTR) DNS Resource Record\n- [RFC 3596](https://tools.ietf.org/html/rfc3596): DNS Extensions to Support IP Version 6\n- [RFC 4034](https://tools.ietf.org/html/rfc4034): Resource Records for the DNS Security Extensions\n- [RFC 4255](https://tools.ietf.org/html/rfc4255): Using DNS to Securely Publish Secure Shell (SSH) Key Fingerprints\n- [RFC 4408](https://tools.ietf.org/html/rfc4408): Sender Policy Framework (SPF) for Authorizing Use of Domains in E-Mail, Version 1\n- [RFC 5155](https://tools.ietf.org/html/rfc5155): DNS Security (DNSSEC) Hashed Authenticated Denial of Existence\n- [RFC 6844](https://tools.ietf.org/html/rfc6844): DNS Certification Authority Authorization (CAA) Resource Record\n- [RFC 6891](https://tools.ietf.org/html/rfc6891): Extension Mechanisms for DNS (EDNS(0))\n- [RFC 7553](https://tools.ietf.org/html/rfc7553): The Uniform Resource Identifier (URI) DNS Resource Record\n\nFor Windows, the package [windns](https://hackage.haskell.org/package/windns)\nprovides a compatible subset of this package's API.";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      tests = {
        "resolv." = {
          depends = [
            (hsPkgs."resolv" or (errorHandler.buildDepError "resolv"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/resolv-0.1.2.0.tar.gz";
      sha256 = "81a2bafad484db123cf8d17a02d98bb388a127fd0f822fa022589468a0e64671";
      });
    }) // {
    package-description-override = "cabal-version:       2.2\r\n\r\nname:                resolv\r\nversion:             0.1.2.0\r\nx-revision: 5\r\n\r\nsynopsis:            Domain Name Service (DNS) lookup via the libresolv standard library routines\r\ndescription: {\r\n\r\nThis package implements an API for accessing\r\nthe [Domain Name Service (DNS)](https://tools.ietf.org/html/rfc1035)\r\nresolver service via the standard @libresolv@ system library (whose\r\nAPI is often available directly via the standard @libc@ C library) on\r\nUnix systems.\r\n.\r\nThis package also includes support for decoding message record types\r\nas defined in the following RFCs:\r\n.\r\n- [RFC 1035](https://tools.ietf.org/html/rfc1035): Domain Names - Implementation And Specification\r\n- [RFC 1183](https://tools.ietf.org/html/rfc1183): New DNS RR Definitions\r\n- [RFC 2782](https://tools.ietf.org/html/rfc2782): A DNS RR for specifying the location of services (DNS SRV)\r\n- [RFC 2915](https://tools.ietf.org/html/rfc2915): The Naming Authority Pointer (NAPTR) DNS Resource Record\r\n- [RFC 3596](https://tools.ietf.org/html/rfc3596): DNS Extensions to Support IP Version 6\r\n- [RFC 4034](https://tools.ietf.org/html/rfc4034): Resource Records for the DNS Security Extensions\r\n- [RFC 4255](https://tools.ietf.org/html/rfc4255): Using DNS to Securely Publish Secure Shell (SSH) Key Fingerprints\r\n- [RFC 4408](https://tools.ietf.org/html/rfc4408): Sender Policy Framework (SPF) for Authorizing Use of Domains in E-Mail, Version 1\r\n- [RFC 5155](https://tools.ietf.org/html/rfc5155): DNS Security (DNSSEC) Hashed Authenticated Denial of Existence\r\n- [RFC 6844](https://tools.ietf.org/html/rfc6844): DNS Certification Authority Authorization (CAA) Resource Record\r\n- [RFC 6891](https://tools.ietf.org/html/rfc6891): Extension Mechanisms for DNS (EDNS(0))\r\n- [RFC 7553](https://tools.ietf.org/html/rfc7553): The Uniform Resource Identifier (URI) DNS Resource Record\r\n.\r\nFor Windows, the package [windns](https://hackage.haskell.org/package/windns)\r\nprovides a compatible subset of this package's API.\r\n}\r\n\r\nlicense:             GPL-2.0-or-later\r\nlicense-files:       LICENSE LICENSE.GPLv2 LICENSE.GPLv3\r\nauthor:              Herbert Valerio Riedel\r\nmaintainer:          hvr@gnu.org\r\ncategory:            Network\r\nbuild-type:          Configure\r\nbug-reports:         https://github.com/hvr/resolv/issues\r\nextra-source-files:  ChangeLog.md\r\n\r\nextra-source-files:  cbits/hs_resolv.h\r\n                     cbits/hs_resolv_config.h.in\r\n                     testdata/msg/*.bin\r\n                     testdata/msg/*.show\r\n                     resolv.buildinfo.in\r\n                     configure\r\n\r\nextra-tmp-files:     autom4te.cache\r\n                     config.log\r\n                     config.status\r\n                     resolv.buildinfo\r\n                     cbits/hs_resolv_config.h\r\n\r\ntested-with:\r\n  GHC ==8.10.1\r\n   || ==8.8.3\r\n   || ==8.6.5\r\n   || ==8.4.4\r\n   || ==8.2.2\r\n   || ==8.0.2\r\n   || ==7.10.3\r\n   || ==7.10.1\r\n   || ==7.8.4\r\n   || ==7.6.3\r\n   || ==7.4.2\r\n\r\nsource-repository head\r\n  type:              git\r\n  location:          https://github.com/hvr/resolv.git\r\n\r\nlibrary\r\n  default-language:  Haskell2010\r\n  other-extensions:  BangPatterns\r\n                     CApiFFI\r\n                     CPP\r\n                     DeriveDataTypeable\r\n                     DeriveFoldable\r\n                     DeriveFunctor\r\n                     DeriveTraversable\r\n                     GeneralizedNewtypeDeriving\r\n                     OverloadedStrings\r\n                     RecordWildCards\r\n                     Trustworthy\r\n\r\n  hs-source-dirs:    src\r\n  exposed-modules:   Network.DNS\r\n  other-modules:     Network.DNS.Message\r\n                     Network.DNS.FFI\r\n                     Compat\r\n\r\n  -- we need binary-0.7.3 for isolate\r\n  build-depends:     base               >= 4.5 && <4.18\r\n                   , base16-bytestring ^>= 0.1 || ^>=1.0.0.0\r\n                   , binary            ^>=0.7.3 || ^>= 0.8\r\n                   , bytestring        ^>=0.9.2 || ^>= 0.10 || ^>= 0.11\r\n                   , containers        ^>=0.4.2.1 || ^>= 0.5 || ^>= 0.6\r\n\r\n  ghc-options:       -Wall\r\n  include-dirs:      cbits\r\n\r\ntest-suite resolv.\r\n  default-language:    Haskell2010\r\n  hs-source-dirs:      src-test\r\n  main-is:             Tests1.hs\r\n  type:                exitcode-stdio-1.0\r\n\r\n  -- dependencies whose version constraints are inherited via lib:resolv component\r\n  build-depends: resolv\r\n               , base\r\n               , bytestring\r\n\r\n  -- additional dependencies not inherited\r\n  build-depends: tasty        ^>= 1.2.3 || ^>=1.3.1\r\n               , tasty-hunit  ^>= 0.10.0\r\n               , directory    ^>= 1.1.0 || ^>= 1.2.0 || ^>= 1.3.0\r\n               , filepath     ^>= 1.3.0 || ^>= 1.4.0\r\n";
    }