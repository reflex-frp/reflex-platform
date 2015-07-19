Hacking on Try Reflex
=====================

To work on a package, use this:

```
./work-on ghcjs reflex-dom
```

You can replace `ghcjs` with `ghc` to hack on the native GHC version of the package (including with GHCi if you want), and you can replace `reflex-dom` with another package name to work on a different package.  You can also use a path to a nix script instead of a package name, as long as the path includes a slash ('/') character (use './' if you want to refer to a script in your current directory).

Once inside the shell, you'll have access to the environment in which the package is built with a few more tools (such as cabal-install) added.
