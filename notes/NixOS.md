### Enabling the binary cache on NixOS

When using Nix on NixOS, only root can add binary caches to the system.  This will force ghcjs-setup to rebuild GHCJS from scratch, which takes hours.  To enable the binary cache, you can add the following line to your `/etc/nixos/configuration.nix`:

```
  nix.trustedBinaryCaches = [ "https://ryantrinkle.com:5443" ];
```

If you already have a trustedBinaryCaches option set up, just add `https://ryantrinkle.com:5443/` to the existing list.

Once it's been added, run `sudo -i nixos-rebuild switch` to make the change take effect, then run ghcjs-setup as normal.

#### Binary cache signing

The public key for the binary cache is:

```
ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
```
