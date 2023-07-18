{
  pkgs = hackage:
    {
      packages = {
        Cabal-syntax.revision = import ./cabal-files/Cabal-syntax.nix;
        bytestring.revision = (((hackage.bytestring)."0.10.12.0").revisions).default;
        safe-exceptions.revision = import ./cabal-files/safe-exceptions.nix;
        echo.revision = import ./cabal-files/echo.nix;
        echo.flags.example = false;
        exceptions.revision = (((hackage.exceptions)."0.10.4").revisions).default;
        directory.revision = (((hackage.directory)."1.3.6.0").revisions).default;
        tar.revision = import ./cabal-files/tar.nix;
        tar.flags.old-bytestring = false;
        tar.flags.old-time = false;
        filepath.revision = (((hackage.filepath)."1.4.2.1").revisions).default;
        network.revision = import ./cabal-files/network.nix;
        network.flags.devel = false;
        mtl.revision = (((hackage.mtl)."2.2.2").revisions).default;
        HTTP.revision = import ./cabal-files/HTTP.nix;
        HTTP.flags.warp-tests = false;
        HTTP.flags.network-uri = true;
        HTTP.flags.conduit10 = false;
        HTTP.flags.warn-as-error = false;
        zlib.revision = import ./cabal-files/zlib.nix;
        zlib.flags.non-blocking-ffi = false;
        zlib.flags.bundled-c-zlib = false;
        zlib.flags.pkg-config = false;
        ghc-prim.revision = (((hackage.ghc-prim)."0.6.1").revisions).default;
        base16-bytestring.revision = import ./cabal-files/base16-bytestring.nix;
        parsec.revision = (((hackage.parsec)."3.1.14.0").revisions).default;
        lukko.revision = import ./cabal-files/lukko.nix;
        lukko.flags.ofd-locking = true;
        Cabal.revision = import ./cabal-files/Cabal.nix;
        splitmix.revision = import ./cabal-files/splitmix.nix;
        splitmix.flags.optimised-mixer = false;
        ed25519.revision = import ./cabal-files/ed25519.nix;
        ed25519.flags.test-hlint = true;
        ed25519.flags.test-properties = true;
        ed25519.flags.no-donna = true;
        ed25519.flags.test-doctests = true;
        containers.revision = (((hackage.containers)."0.6.5.1").revisions).default;
        stm.revision = (((hackage.stm)."2.5.0.1").revisions).default;
        regex-posix.revision = import ./cabal-files/regex-posix.nix;
        regex-posix.flags._regex-posix-clib = false;
        network-uri.revision = import ./cabal-files/network-uri.nix;
        base.revision = (((hackage.base)."4.14.3.0").revisions).default;
        time.revision = (((hackage.time)."1.9.3").revisions).default;
        hackage-security.revision = import ./cabal-files/hackage-security.nix;
        hackage-security.flags.cabal-syntax = true;
        hackage-security.flags.base48 = true;
        hackage-security.flags.lukko = true;
        hackage-security.flags.use-network-uri = true;
        hackage-security.flags.old-directory = false;
        hackage-security.flags.mtl21 = false;
        hsc2hs.revision = import ./cabal-files/hsc2hs.nix;
        hsc2hs.flags.in-ghc-tree = false;
        async.revision = import ./cabal-files/async.nix;
        async.flags.bench = false;
        base-orphans.revision = import ./cabal-files/base-orphans.nix;
        random.revision = import ./cabal-files/random.nix;
        deepseq.revision = (((hackage.deepseq)."1.4.4.0").revisions).default;
        rts.revision = (((hackage.rts)."1.0.1").revisions).default;
        edit-distance.revision = import ./cabal-files/edit-distance.nix;
        template-haskell.revision = (((hackage.template-haskell)."2.16.0.0").revisions).default;
        binary.revision = (((hackage.binary)."0.8.8.0").revisions).default;
        base64-bytestring.revision = import ./cabal-files/base64-bytestring.nix;
        regex-base.revision = import ./cabal-files/regex-base.nix;
        integer-gmp.revision = (((hackage.integer-gmp)."1.0.3.0").revisions).default;
        process.revision = import ./cabal-files/process.nix;
        unix.revision = (((hackage.unix)."2.7.2.2").revisions).default;
        transformers.revision = (((hackage.transformers)."0.5.6.2").revisions).default;
        cabal-install-solver.revision = import ./cabal-files/cabal-install-solver.nix;
        cabal-install-solver.flags.debug-conflict-sets = false;
        cabal-install-solver.flags.debug-expensive-assertions = false;
        cabal-install-solver.flags.debug-tracetree = false;
        text.revision = (((hackage.text)."1.2.4.1").revisions).default;
        th-compat.revision = import ./cabal-files/th-compat.nix;
        array.revision = (((hackage.array)."0.5.4.0").revisions).default;
        ghc-boot-th.revision = (((hackage.ghc-boot-th)."8.10.7").revisions).default;
        resolv.revision = import ./cabal-files/resolv.nix;
        pretty.revision = (((hackage.pretty)."1.1.3.6").revisions).default;
        hashable.revision = import ./cabal-files/hashable.nix;
        hashable.flags.containers = true;
        hashable.flags.random-initial-seed = false;
        hashable.flags.integer-gmp = true;
        cryptohash-sha256.revision = import ./cabal-files/cryptohash-sha256.nix;
        cryptohash-sha256.flags.exe = false;
        cryptohash-sha256.flags.use-cbits = true;
        };
      compiler = {
        version = "8.10.7";
        nix-name = "ghc8107";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "1.2.4.1";
          "array" = "0.5.4.0";
          "mtl" = "2.2.2";
          "parsec" = "3.1.14.0";
          "bytestring" = "0.10.12.0";
          "filepath" = "1.4.2.1";
          "stm" = "2.5.0.1";
          "ghc-prim" = "0.6.1";
          "ghc-boot-th" = "8.10.7";
          "base" = "4.14.3.0";
          "time" = "1.9.3";
          "directory" = "1.3.6.0";
          "exceptions" = "0.10.4";
          "rts" = "1.0.1";
          "transformers" = "0.5.6.2";
          "template-haskell" = "2.16.0.0";
          "deepseq" = "1.4.4.0";
          "unix" = "2.7.2.2";
          "integer-gmp" = "1.0.3.0";
          "binary" = "0.8.8.0";
          "containers" = "0.6.5.1";
          };
        };
      };
  extras = hackage:
    { packages = { cabal-install = ./.plan.nix/cabal-install.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "cabal-install" = {
            flags = {
              "lukko" = lib.mkOverride 900 true;
              "native-dns" = lib.mkOverride 900 true;
              };
            };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "Cabal-syntax".components.library.planned = lib.mkOverride 900 true;
          "base16-bytestring".components.library.planned = lib.mkOverride 900 true;
          "echo".components.library.planned = lib.mkOverride 900 true;
          "base-orphans".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "ed25519".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "zlib".components.library.planned = lib.mkOverride 900 true;
          "cryptohash-sha256".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "safe-exceptions".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "cabal-install".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "resolv".components.library.planned = lib.mkOverride 900 true;
          "edit-distance".components.library.planned = lib.mkOverride 900 true;
          "regex-base".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "cabal-install-solver".components.library.planned = lib.mkOverride 900 true;
          "network-uri".components.library.planned = lib.mkOverride 900 true;
          "regex-posix".components.library.planned = lib.mkOverride 900 true;
          "HTTP".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "async".components.library.planned = lib.mkOverride 900 true;
          "th-compat".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "tar".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "hackage-security".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "lukko".components.library.planned = lib.mkOverride 900 true;
          "base64-bytestring".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "cabal-install".components.exes."cabal".planned = lib.mkOverride 900 true;
          };
        })
    ];
  }