### Enabling the binary cache on NixOS

When using Nix on NixOS, only root can add binary caches to the system.  This will force `try-reflex` to rebuild GHCJS from scratch, which takes hours.  To use the binary cache, you can add the following lines to your `/etc/nixos/configuration.nix`:

```
nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
```

If you already have one of these variables set up, just add these values to the existing lists.

Once it's been added, run `sudo nixos-rebuild switch` to make the change take effect, then run `./try-reflex` as normal.

Note: If you'd prefer not to use `nixcache.reflex-frp.org` by default on your system, you can add it to `nix.trustedBinaryCaches` instead of `nix.binaryCaches`.  This way, scripts like `try-reflex` will be allowed to use it, but other nix commands will ignore it.  Once it's in trustedBinaryCaches, you can always pass `--option extra-binary-caches https://nixcache.reflex-frp.org` to nix commands such as `nix-build` and `nix-shell` manually if you'd like to use it for a particular build.

Note: If the binary cache does not appear to be applying and you are seeing messages like
```
warning: substituter 'https://nixcache.reflex-frp.org' does not have a valid signature for path '/nix/store/...'
```
check (and consider removing or modifying) `$HOME/.config/nix/nix.conf`, as well.
