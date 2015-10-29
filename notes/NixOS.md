### Enabling the binary cache on NixOS

When using Nix on NixOS, only root can add binary caches to the system.  This will force try-reflex to rebuild GHCJS from scratch, which takes hours.  To enable the binary cache, you can add the following lines to your `/etc/nixos/configuration.nix`:

```
nix.trustedBinaryCaches = [ "https://ryantrinkle.com:5443" ];
nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
```

Note: If you are running NixOS 14.12 or older, you do not need the `binaryCachePublicKeys` line.

If you already have one of these variables set up, just add these values to the existing lists.

Once it's been added, run `sudo nixos-rebuild switch` to make the change take effect, then run try-reflex as normal.
