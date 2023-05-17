Reflex Platform
===============

NOTE: We don't support STACK anymore, please use cabal

Reflex Platform is a curated package set and set of tools that let you build Haskell packages so they can run on a variety of platforms.
Reflex Platform is built on top of the [nix](https://nixos.org/nix/) package manager.

There are five main reasons to use Reflex Platform:

1. It's curated: the core packages in Reflex Platform are known to work together and are tested together.

2. It's cached: the core packages in Reflex Platform are cached so you can download prebuilt binaries from the public cache instead of building from scratch.

3. It's consistent: nix locks down dependencies even outside the Haskell ecosystem (e.g., versions of C libraries that the Haskell code depends on), so you get completely reproducible builds.

4. It's cross-platform: Reflex Platform is designed to target iOS and Android on mobile, JavaScript on the web, and Linux and macOS on desktop. It's Haskell, everywhere.

5. It's convenient: Reflex Platform comes packaged with tools to make development easier, like a [hoogle](https://hoogle.haskell.org/) server that you can run locally to look up definitions.

To get started with Reflex development, follow the instructions below.

Try Reflex lets you set up an environment from which you can use [Reflex](https://github.com/ryantrinkle/reflex) with GHC or [GHCJS](https://github.com/ghcjs/ghcjs).

To use Reflex Platform as a build/development system for your own projects, refer to `HACKING.md`.

To see what has changed since a previous version of Reflex Platform, see `ChangeLog.md`.

Important Notes
---------------

### Interface documentation

All of the interface documentation lives in [docs](./docs)


### Branches

The default branch for this git repo is `develop`, but the latest release is `master`.
End-users should prefer to use `master`, *not* `develop`.

The default branch is just `develop` to ensure new PRs go to the right place by default.

### OS Compatibility

We support:
  * NixOS
  * MacOS 

#### Windows

Reflex Platform will not work on Windows because we rely on [Nix](https://nixos.org/nix/) to define and construct our environment. You may have some success by using the Windows Subsystem for Linux 2, but we do not provide support for this platform.

### Memory Requirements

GHCJS uses a lot of memory during compilation.
16GB of memory is recommended for ghc, with 8GB being pretty close to bare minimum.
32GB+ of memory is recommended for ghcjs

Setup
-----
This process will install the [Nix package manager](https://nixos.org/nix/).
If you prefer to install Nix yourself, you may do so any time prior to step 2.

1. Clone this repository:

    ```bash
    git clone https://github.com/reflex-frp/reflex-platform
    ```

1. Navigate into the `reflex-platform` folder and run the `try-reflex` command.
   This will install Nix, if you don't have it already, and use it to wrangle all the dependencies you'll need and drop you in an environment from which you can use Reflex.
   Be warned, this might take a little while the first time (but it shouldn't take more than a few minutes, if your binary cache is configured properly):

    ```bash
    cd reflex-platform/example
    nix-shell -A shells.ghc # To get into a shell with GHC
    nix-shell -A shells.ghcjs # To get into a shell with GHCJS
    ```

1. From this nix-shell, you can compile any haskell source files you like.
   Replace `your-source-file.hs` with the name of the file you'd like to compile.
   For the most part, ghcjs supports the same options as ghc:

   * GHC
     ```bash
     ghc --make your-source-file.hs
     ./your-source-file
     ```
     Compilation will produce a `your-source-file` native executable via [WebkitGtk](https://github.com/WebKit/webkit).
     Simply run it to launch your app.
Developer tools are available via `Inspect Element` in the right-click context menu.

   * GHCJS
     ```bash
     ghcjs --make your-source-file.hs
     ```
     Compilation will produce a `your-source-file.jsexe` folder containing an `index.html` file.
     Open that in your browser to run your app.

**Don't use** `cabal install` to install libraries while inside the try-reflex shell - the resulting libraries may not be found properly by ghc or ghcjs.
Using Cabal to configure, build, test, and run a particular package, however, should work just fine.

`try-reflex` and `ghcjs --make` are not recommended for real-world projects — just as a quick and easy way to install Nix and experiment with `reflex-dom`.
If you need to use additional Haskell libraries (e.g. from Hackage), we recommend using the tools described in [project-development.rst](docs/project-development.rst) instead.

Haddock
----
If you've already set up nix, haddock documentation for the versions pinned by your current reflex-plaftorm can be browsed by running

```bash
./scripts/docs-for reflex
./scripts/docs-for reflex-dom
```

