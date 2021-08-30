Multi-package Reflex example
---

This repo is an example of combining `cabal.project`, Nix,
`reflex-platform`, and `jsaddle` to drastically improve the
developer experience.

Either clone this repo with `--recurse-submodules`, or run `git
submodule update --init --recursive` in this directory after cloning
to make sure `reflex-platform` is checked out.

First, run `./reflex-platform/try-reflex` at least once. We won't use
it at all in this project, but it does some extra work to setup your
system requirements automatically, namely installing Nix and
configuring the Reflex binary cache.

Once Nix is installed, everything else is mostly handled for you. To
build the project's backend and `jsaddle-webkit2gtk` frontend app, use
the `./cabal` script:

```bash
$ ./cabal new-build all
```

To build the GHCJS app, use the `./cabal-ghcjs` script:

```bash
$ ./cabal-ghcjs new-build all
```

You can use GHCi with the `jsaddle-webkit2gtk` app for much better dev
cycles:

```bash
$ ./cabal new-repl frontend
```

`nix-build`
---

Nix is useful for creating deterministic, production ready build
products. You can use the `nix-build` command to build all the parts
of the project with Nix.

- Build everything

  ```bash
  $ nix-build
  trace:

  Skipping ios apps; system is x86_64-linux, but x86_64-darwin is needed.
  Use `nix-build -A all` to build with remote machines.
  See: https://nixos.org/nixos/manual/options.html#opt-nix.buildMachines


  /nix/store/5p041yq3ldniji7xizrxihmhmr576vah-reflex-project

  $ tree result
  result
  ├── android
  │   └── frontend -> /nix/store/4w62ly3hi75zpdmiq52lk1m4kir660vc-android-app
  ├── ghc
  │   ├── backend -> /nix/store/bgraikacjv68lfcghkprj3mspwx9f2bn-backend-0.1.0.0
  │   ├── common -> /nix/store/lcgz36j77y6w7jyd39b14zp00hfaxn3s-common-0.1.0.0
  │   └── frontend -> /nix/store/h9dbc2dvh11g1saj52ndn8ys3kj6f03l-frontend-0.1.0.0
  └── ghcjs
      ├── common -> /nix/store/fgbmn6mjgh7gfdbgnb7a21fsb9175gmv-common-0.1.0.0
      └── frontend -> /nix/store/hfpq2580jbvgm20p992v8qjdczvr20gm-frontend-0.1.0.0

  9 directories, 0 files
  ```

- Build the backend

  ```bash
  $ nix-build -o backend-result -A ghc.backend
  ```

- Build the JS app

  ```bash
  $ nix-build -o frontend-result -A ghcjs.frontend
  ```

- Build the native frontend

  ```bash
  $ nix-build -o native-frontend-result -A ghc.frontend
  ```

Motivation
---

Building a multi-package project with Nix can be a pain because of
Nix's lack of incremental building. A small change to a common package
will require Nix to rebuild that package from scratch, causing a huge
interruption during development. Although this is usually where Stack
would shine, Stack doesn't officially support using Nix for Haskell
derivations, and has zero support for Nix with GHCJS. You *can* build
Reflex apps using only Stack and no Nix, but you lose a lot of
benefits that `reflex-platform` provides, like the curated set of
package versions that Reflex works best with (including a
GHCJS-optimized `text`library), binary caches of all the Haskell
derivations, and zero-effort cross compilation for native mobile apps.

How it works
---

See
[project-development.md](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md).

---

TODO
---

- Actually implement a backend / frontend that uses the `common`
  library to show that even cross-package dependencies are built
  incrementally.
- `new-build` doesn't yet support any means of programmatically
  finding build products. It would be nice to have some kind of
  solution for this, especially so that the backend could serve the
  `dist-ghcjs` products without some hardcoded path.
