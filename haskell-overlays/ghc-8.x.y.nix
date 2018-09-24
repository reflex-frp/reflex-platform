{ haskellLib }:

self: super: {
  ghcjs-prim = null;
  concurrent-output = haskellLib.doJailbreak super.concurrent-output;
  base-compat= self.callHackage "base-compat" "0.9.3" {};
}
