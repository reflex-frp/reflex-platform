### Adding the binary cache on NixOS

When using Nix on NixOS, only root can add binary caches to the
system. Without this cache, try-reflex will rebuild GHCJS from scratch,
which can take hours. To add this binary cache, add the
following lines to your `/etc/nixos/configuration.nix`:

```
nix.trustedBinaryCaches = [ "https://ryantrinkle.com:5443" ];
nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
```

Note: If you are running NixOS 14.12 or older, you do not need the
`nix.binaryCachePublicKeys` line.

If you already have one of these variables set up, just add these
values to the existing lists.

Once it's been added, run `sudo nixos-rebuild switch` to make the
change take effect, then run try-reflex as normal.
