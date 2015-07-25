Hacking on Try Reflex
=====================

To work on a particular package, use the work-on script **instead of** try-reflex:

```
./work-on ghcjs reflex-dom
```

This will give you the exact environment needed to work with the given package and platform, rather than the general-purpose environment provided by try-reflex.

You can replace `ghcjs` with `ghc` to hack on the native GHC version of the package (including with GHCi if you want), and you can replace `reflex-dom` with another package name to work on a different package.  You can also use a path to a nix script instead of a package name, as long as the path includes a slash ('/') character (use './' if you want to refer to a script in your current directory).

Once inside the shell, you'll have access to the environment in which the package is built with a few more tools (such as cabal-install) added.

Checking out a sub-repo
-----------------------

The `hack-on` script is provided for conveniently checking out a sub-repository.

```
./hack-on reflex
```

This will check out the same version of `reflex` currently being used by `try-reflex`.  Note that `reflex`, here, is a path relative to the current directory, so you must be in the try-reflex folder when you execute this.

Once the repository is checked out, you can make modifications to it, which will be used the next time you enter a `try-reflex` shell.  Existing `try-reflex` sessions will not be affected.

When you are done hacking on the sub-repository, you can use the `hack-add` script to check your changes into try-reflex without needing to delete the repository, like so:

```
./hack-add reflex
```
