# Mars/Reflex-platform

NOTE: **Warning this is new and experimental**

Mars/Reflex platform is a wrapper/project on top of haskell.nix that adds a few things to haskell.nix, this includes but not limited to:

- GHC Splices
- GHCJS JSString
- Easy Mobile builds
- Multiple platform support

## OS Compatibility

Any OS that you can install `nix` on should function with this 100%.

### General Requirements

- `nix` 2.13+, this is a hard requirement due to haskell.nix

### Windows

We don't support WSL2, though it will more than likely work on Windows 11!

### Memory Requirements

For `ghc` only builds 16GB is recommended

For `ghcjs` builds we recommend at least 32GB, due to a bug in GHCJS 8.10 that spikes memory usage

## Supported GHCs

We currently only support GHC 8.10.7, though you can use any compiler that is present in haskell.nix, they probably won't be cached, and you lose TemplateHaskell splicing support if you aren't using `ghc8107Splices`

We also currently only support `cabal` based builds, `stack` project building is not implemented, and probably won't be implemented for awhile

## Setup

install [nix](https://nixos.org/download.html) and follow `./doc/Project.md` and the relevant haskell.nix documentation!

## Upgrade Path

If you're currently using `obelisk` there is a very WIP branch available on the obelisk git/github, available [here](https://github.com/obsidiansystems/obelisk.git)

Otherwise please look at [haskell.nix documentation](https://input-output-hk.github.io/haskell.nix/) and `./doc/Project.md` for more info on the new interface.
