{ haskellLib
, fetchFromGitHub
, nixpkgs
}:

with haskellLib;

self: super: {

  # Need an older version for GHC 8.6
  haddock-api = dontHaddock (doJailbreak (self.callHackage "haddock-api" "2.22.0" {}));
  # TODO this conflicts with the pandoc version
  # haddock-library = doJailbreak (self.callHackage "haddock-library" "1.7.0" {});

  # Fixing things that are marked broken in 20.09:
  constrained-dynamic = dontCheck (markUnbroken super.constrained-dynamic);
  haven = doJailbreak (markUnbroken super.haven);

  # These take over an hour to run, each
  cryptonite = dontCheck super.cryptonite;
  scientific = dontCheck (self.callHackage "scientific" "0.3.7.0" {});

  # pandoc 2.16 and dependencies
  pandoc = self.callHackage "pandoc" "2.16.2" {};
  citeproc = self.callHackage "citeproc" "0.6" {};
  hslua-marshalling = self.callHackage "hslua-marshalling" "2.0.1" {};
  hslua-module-path  = self.callHackage "hslua-module-path" "1.0.0" {};
  hslua-module-version  = self.callHackage "hslua-module-version" "1.0.0" {};
  lpeg = self.callHackage "lpeg" "1.0.1" {};
  tasty-bench = self.callHackage "tasty-bench" "0.3.1" {};
  unicode-collation = self.callHackage "unicode-collation" "0.1.3.1" {};

  # beam packages
  beam-core = self.callHackage "beam-core" "0.10.0.0" {};
  beam-migrate = self.callHackage "beam-migrate" "0.5.2.0" {};
  beam-postgres = haskellLib.dontCheck (self.callHackage "beam-postgres" "0.5.3.0" {});
  beam-automigrate = self.callHackage "beam-automigrate" "0.1.3.0" {};

  # hnix 0.12 and dependencies
  hnix = dontCheck
    (overrideCabal (self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix) {}) (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ nixpkgs.nix ];
      testHaskellDepends = (drv.testHaskellDepends or []) ++ [ nixpkgs.nix super.criterion ];
    }));
  hnix-store-core =
    self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix-store + "/hnix-store-core") {};
  hnix-store-remote =
    self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix-store + "/hnix-store-remote") {};
  data-fix = self.callHackage "data-fix" "0.3.2" {};
  neat-interpolation = self.callHackage "neat-interpolation" "0.4" {};
  prettyprinter = self.callHackage "prettyprinter" "1.7.0" {};
  cryptohash-sha512 = doJailbreak super.cryptohash-sha512;
  ListLike = self.callHackage "ListLike" "4.7.3" {};

  # ghcjs-promise is marked broken in nixpkgs
  ghcjs-promise = self.callHackage "ghcjs-promise" "0.1.0.3" {};

  utf8-string = self.callHackage "utf8-string" "1.0.1.1" {};

  ghcjs-base = doJailbreak (self.callHackage "ghcjs-base" "0.2.1.0" {});
  hlint = self.callHackage "hlint" "3.2.8" {};

  # aeson 2 support
  # jailbreak to allow older primitive version
  aeson = doJailbreak (self.callHackage "aeson" "2.1.1.0" {});
  aeson-pretty = overrideCabal (self.callHackage "aeson-pretty" "0.8.9" {}) (drv: {
    jailbreak = true;
    # support our version of unordered-containers
    preConfigure = ''
      substituteInPlace Data/Aeson/Encode/Pretty.hs \
        --replace "import qualified Data.HashMap.Strict as H (toList, mapKeys)" "import qualified Data.HashMap.Strict as H (toList)"
    '';
  });
  hashable = self.callHackage "hashable" "1.3.5.0" {};
  generically = self.callHackage "generically" "0.1" {};
  OneTuple = self.callHackage "OneTuple" "0.3.1" {};
  attoparsec = self.callHackage "attoparsec" "0.14.4" {};
  quickcheck-instances = self.callHackage "quickcheck-instances" "0.3.28" {};
  vector = self.callHackage "vector" "0.12.3.1" {};
  indexed-traversable = self.callHackage "indexed-traversable" "0.1.2" {};
  semialign = self.callHackage "semialign" "1.2.0.1" {};
  text-short = self.callHackage "text-short" "0.1.5" {};
  time-compat = self.callHackage "time-compat" "1.9.6.1" {};
  base-orphans = self.callHackage "base-orphans" "0.8.7" {};
  yaml = self.callHackage "yaml" "0.11.7.0" {};
  websockets = self.callHackage "websockets" "0.12.7.3" {};
  http2 = self.callHackage "http2" "3.0.3" {};
  HsYAML-aeson = self.callHackage "HsYAML-aeson" "0.2.0.1" {};
  fsnotify = self.callHackage "fsnotify" "0.4.1.0" {};
  unliftio = self.callHackage "unliftio" "0.2.24.0" {};
  sandwich = self.callHackage "sandwich" "0.1.0.6" {};
}
