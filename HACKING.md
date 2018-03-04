Hacking on the Reflex Platform
==============================

To work on a particular package, use the work-on script **instead of** `./try-reflex`:

```
~/reflex-platform/scripts/work-on ghcjs ./your-project
```

This will use your package's cabal file to determine dependencies.  If you have a default.nix, it will use that instead.  Note that your project's path must include at least one slash ('/') so that work-on can detect that it is a path, rather than a package name.

This will give you the exact environment needed to work with the given package and platform, rather than the general-purpose environment provided by the Reflex Platform.

You can replace `ghcjs` with `ghc` to hack on the native GHC version of the package (including with GHCi if you want).  You can also use a package name instead of a path, which will drop you into the standard build environment of that package; this works even if you don't yet have the source for that package.

Instead of specifying `ghcjs` or `ghc` for the platform, you can also specify a path to a nix expression file whose value represents a Haskell environment, for example:

```
{ reflex-platform, ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    some-package = self.callPackage ./some-package {};
  };
}
```

Finally, you can specify a parenthesized expression as your platform, which will be interpreted as a raw nix expression.  For example, `(import ./my-platform.nix {})`.

Once inside the shell, you'll have access to the environment in which the package is built with a few more tools (such as cabal-install) added.

Checking out a sub-repo
-----------------------

The `hack-on` script is provided for conveniently checking out a sub-repository.

```
./scripts/hack-on reflex
```

This will check out the same version of `reflex` currently being used by the Reflex Platform.  Note that `reflex`, here, is a path relative to the current directory, so you must be in this folder when you execute this.

Once the repository is checked out, you can make modifications to it, which will be used the next time you enter a try-reflex or work-on shell.  Existing sessions will not be affected—exit and re-enter your shell to use the changes.

When you have completed some work hacking on the sub-repository, you can use the `hack-add` script to check your changes into this repository without needing to delete the repository, like so:

```
./scripts/hack-add reflex
```

You can then commit and push reflex-platform without needing to delete the sub-repository.

When you are completely done with a sub-repository, you can remove it using `hack-off`:

```
./scripts/hack-off reflex
```

This will remove the repository and replace the `default.nix` and `git.json` files that were pointing to it before running `hack-on`.
