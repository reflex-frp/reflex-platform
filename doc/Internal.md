# Internal Documentation

This is for people that are familiar with nix, and are willing to hack on mars.

## Cross Compilation

This will cover a few sections, including splices, and how we setup cross-compilation

### Splices

Splices are set up by looking at the host package-set, and setting up the loading of splices based on that package-set. We still run the cabal-solver for the target platform since it can spit out different flag configuration for each platform. We don't splice some specific packages due to this discrepancy. A tiny list would be `[ "android-activity" "cabal" ]`.

### Cross Driver

We don't use haskell.nix's implementation of cross-compiling since it seems to be buggy at times. We currently define the cross-driver in ./modules/cross-driver.nix.

This module defines how to set-up the project for cross-compiling. We currently have hacks for both iOS and GHCJS due to some bugs that we've found.

iOS has a bug with at least GHC 8.10.7 where the linker crashes when we try to link the final executable. We fix this by passing `-fwhole-archive-hs-libs` to GHC so it doesn't dead-strip the resulting binary

We have custom configuration for our fork/modification to GHCJS so we have to make sure that we don't forget about packages we add into the compiler. This is why we have to set `reinstallableLibGhc` to false, alongside re-setting up `nonReinstallablePkgs`

## Hackage

We have a hackage-driver that spits out a "fake" hackage that the cabal-solver can use and solve with. This is not recommended to use for anything that can be shoved into a cabal.project.

We write a "dummy" json that cabal can understand. Then we pack that up into a tarball that we eventually pass to haskell.nix `tools` which helpfully generates the nix bindings for the repo for us!


## Compilers

We provide our own versions of our compilers, please take a look into `./modules/overlays/compilers.nix`, also take a look at `./modules/overlays/haskell.nix`
