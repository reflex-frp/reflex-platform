Project Development
---

A common structure for Reflex-DOM applications is to have a `common`
package shared by a `backend` and a `frontend`. To accomplish this
with `reflex-platform`, create the three cabal packages, and then make
a project file called `default.nix`.

```nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
```

See [project/default.nix](../project/default.nix) for more details on
available options.

Building with Nix
---

With this project file, Nix can be used to build each of the packages.

```bash
$ nix-build -o backend-result -A ghc.backend
$ nix-build -o frontend-result -A ghcjs.frontend
```

These commands will create two symlinks (`backend-result` and
`frontend-result`) that point at the build products in the Nix store.

Building with Cabal
---

`nix-build` is great for release builds, but it is not an incremental
build system. Changing one file will require `nix-build` to recompile
the entire package. In order to get a dev environment where changing a
module only rebuilds the affected modules, even across packages, a
more incremental tool is required.

`cabal-install` is the only tool that simultaneously supports Nix and
GHCJS. The Nix expression in the project file uses `shells` to setup
`nix-shell` sandboxes that Cabal can use to build your project. The
`shells` field defines which platforms we'd like to develop for, and
which packages' dependencies we want available in the development
sandbox for that platform. Note that specifying `common` is important;
otherwise it will be treated as a dependency that needs to be built by
Nix for the sandbox. You can use these shells with `cabal.project`
files to build all three packages in a shared incremental environment,
for both GHC and GHCJS.

```yaml
-- cabal.project
packages:
  common/
  backend/
  frontend/
```

```yaml
-- cabal-ghcjs.project
compiler: ghcjs
packages:
  common/
  frontend/
```

To build with GHC, use the `nix-shell` command to enter the sandbox
shell and use `cabal` (which is supplied by the sandbox):

```bash
$ nix-shell -A shells.ghc
[nix-shell:~/path]$ cabal new-build all
```

To build with GHCJS:

```bash
$ nix-shell -A shells.ghcjs
[nix-shell:~/path]$ cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

You can also run commands in the nix-shell without entering it
interactively using the `--run` mode. This is useful for scripting.

```bash
$ nix-shell -A shells.ghc --run "cabal new-build all"
$ nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"
```

**Note:** Cabal may complain with `Warning: The package list for
'hackage.haskell.org' does not exist. Run 'cabal update' to download
it.` This can be ignored since we are using Nix instead of Cabal's
solver. Nix uses a package snapshot similar to a Stackage LTS.

Building frontends with GHC
---

GHCJS can be quite slow, especially if you are using Template
Haskell. Building the frontend with GHC can drastically speed up build
times, and enables you to test from GHCi for even faster reloads.

JSaddle is a set of libraries that allows Reflex-DOM to swap out its
JavaScript backend easily. By default, Reflex-DOM's `mainWidget` will
work on GHC out of the box, using the `jsaddle-webkit2gtk` backend. So
simply building your `frontend` package using GHC will produce a
working native program that renders DOM using WebKit. This is
recommended for native desktop releases.

`jsaddle-warp` is an alternative JSaddle backend that uses a local
`warp` server and WebSockets to control a browser from a native
Haskell process. This is recommended to allow testing different
browsers, and to make use of a browser's significantly better
developer tools.

To use it, add `jsaddle-warp` and `reflex-dom-core` to your frontend's
dependencies, and change `main` like so:

```haskell
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (mainWidget)

main :: IO ()
main = run 3911 $ mainWidget app
```

This will spawn the Warp server on port 3911, which you can connect
your browser to to run the app. It will also compile under GHCJS as
is, automatically defaulting back to the GHCJS backend. Both
`jsaddle-warp` and `jsaddle-webkit2gtk` are safe to use from GHCi, so
you can test changes even more quickly with `:r`.

**Note:** The native backends for JSaddle have much much better
runtime performance than the GHCJS backend. To put it in perspective,
the native backends running on most mobile phones will outperform most
desktops running the GHCJS backend. GHCJS is quite fast, especially
considering all it has to do; but native Haskell is simply much faster
than a JS VM for what Reflex is doing.

Building mobile apps
---

The project Nix expression also supports defining mobile apps.

```nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };
  
  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };
})
```

Build them with `nix-build`:

```bash
$ # On Linux
$ nix-build -o android-result -A android.frontend
$ # On macOS
$ nix-build -o ios-result -A ios.frontend
```
