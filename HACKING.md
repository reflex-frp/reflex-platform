# Hacking on the Reflex Platform

This guide shows you how to use Reflex Platform in your own projects.
Reflex Platform contains a number of useful scripts that make this
process easier for you.

## Working on packages (the work-on script)

To work on a particular package, use the work-on script **instead of**
`./try-reflex`:

```
~/reflex-platform/scripts/work-on ghcjs ./your-project
```

This will use your package's cabal file to determine dependencies. If
you have a default.nix, it will use that instead. Note that your
project's path must include at least one slash ('/') so that work-on
can detect that it is a path, rather than a package name.

This will give you the exact environment needed to work with the given
package and platform, rather than the general-purpose environment
provided by the Reflex Platform. Once inside the resulting shell,
you'll have access to the environment in which the package is built
with a few more tools (such as cabal-install) added.

### Custom values for “platform”

The first argument of work-on corresponds to the Haskell platform.
Many different platform are provided for you. A partial list of
possible platforms include:

- ghc
- ghc8_6
- ghcjs
- ghcjs8_6

In addition, instead of specifying the name of the platform, you can
specify a path to a Nix expression file representing a Haskell
environment.

For example, you can create a file called env.nix like so:

```
{ reflex-platform, ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    some-package = self.callPackage ./some-package {};
  };
}
```

You can now run work-on with this new platform like so:

```
~/reflex-platform/scripts/work-on ./env.nix ./your-project
```

Finally, you can specify a parenthesized expression as your platform,
which will be interpreted as a raw nix expression. For example,
`(import ./my-platform.nix {})`.

### Custom values for “package”

The second argument of work-on corresponds to the Haskell package.
This is meant to be the package that you are wanting to work on.

Package can be a name of a package. In this case, the name will be
looked up in the Hackage database.

Otherwise, this should be a path to the package. Any directory with a
.cabal file should work. In addition, this script will use a .nix file
for the package if it exists.

## Checking out a sub-repo (the hack-on script)

The `hack-on` script is provided for conveniently checking out a
sub-repository.

```
~/reflex-platform/scripts/hack-on haskell-overlays/reflex-packages/dep/reflex
```

This will check out the same version of `reflex` currently being used by the
Reflex Platform. Note that `haskell-overlays/reflex-packages/dep/reflex`, here,
is a path relative to the current directory, so you must be in this folder when
you execute this.

Once the repository is checked out, you can make modifications to it,
which will be used the next time you enter a try-reflex or work-on
shell. Existing sessions will not be affected—exit and re-enter your
shell to use the changes.

When you have completed some work hacking on the sub-repository, you
can use the `hack-add` script to check your changes into this
repository without needing to delete the repository, like so:

```
~/reflex-platform/scripts/hack-add haskell-overlays/reflex-packages/dep/reflex
```

You can then commit and push reflex-platform without needing to delete
the sub-repository.

When you are completely done with a sub-repository, you can remove it
using `hack-off`:

```
~/reflex-platform/scripts/hack-add haskell-overlays/reflex-packages/dep/reflex
```

This will remove the repository and replace the `default.nix` and
`git.json` files that were pointing to it before running `hack-on`.
