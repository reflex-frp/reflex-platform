Hacking on Try Reflex
=====================

To work on a package, use this:

```
nix-shell --pure default.nix -A ghcjs.reflex-dom.env
```

You can replace `ghcjs` with `ghc` to hack on the native GHC version of the package (including with GHCi if you want), and you can replace `reflex-dom` with another package name to work on a different package.

Once inside the shell, you'll have access to the exact environment in which the package is built.  Note that this doesn't include cabal!  You'll need to build using --make, ghci, or something else like that.
