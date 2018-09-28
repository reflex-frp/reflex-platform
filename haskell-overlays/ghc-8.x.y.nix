{ lib, haskellLib }:

self: super: lib.optionalAttrs (lib.versionOlder super.ghc.version "8.4.0") {
  concurrent-output = haskellLib.doJailbreak super.concurrent-output;
  base-compat= self.callHackage "base-compat" "0.9.3" {};
}
