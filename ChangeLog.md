# Revision history for reflex-platform

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released.

## Unreleased

* New pins
  * hnix 0.12.0.1 and dependencies

## v0.7.0.0

* Bump
  * all-cabal-hashes ("Update from Hackage at 2020-12-01T14:56:13Z")
  * beam-core to 0.9.0.0
  * beam-migrate to 0.5.0.0
  * beam-postgres to 0.5.0.0
* New pins
  * [beam-automigrate](https://github.com/obsidiansystems/beam-automigrate) 0.1.0.0
* Android
  * Add allowBackup/fullBackupContent to manifest
  * Bump gradle to v3.4.0 and upgrade related maven dependencies
  * Allow maven dependencies to be overridden
  * Copy google-services.json to the location expected by google integrations
  * Disable multi-dex on android builds (see [note](https://github.com/reflex-frp/reflex-platform/commit/0e3e94467ab117832989632d65cfcc9afd538809))
  * Update to android sdk version 29
* iOS
  * Copy default splash screen images and app icon into sample application
  * Make ios-deploy tool overridable
  * Update iosSdkVersion to 13.2
  * Make xcode version customizable
  * Remove ResourceRules.plist as it is no longer necessary
  * Set CFBundleIconName as required by App Store
  * Add portable-deploy script for iOS for creating a pre-signed tarball which can be tested without Nix.
  * Add iOS Simulator target

## v0.6.1.0

* Bump
  * all-cabal-hashes ("Update from Hackage at 2020-11-06T16:55:57Z")
  * which to 0.2.0.0
  * gargoyle to 0.1.1.0
  * gargoyle-postgresql to 0.2.0.1
  * gargoyle-postgresql-nix to 0.3.0.0
  * aeson-gadt-th to 0.2.4
  * reflex-ghci to 0.1.5.1
  * constraints-extras to 0.3.0.2
  * some to 1.0.1
  * prim-uniq to 0.2
  * aeson-gadt-th to 0.2.4
  * dependent-map to 0.4
  * dependent-sum to 0.7.1.0
  * dependent-sum-template to 0.1.0.3
  * dependent-sum-aeson-orphans to 0.3.0.0
  * reflex to 0.7.2.0
  * reflex-vty to 0.1.4.1
  * patch to 0.0.3.2
  * reflex-dom to 0.6.1.0
  * reflex-fsnotify to 0.2.1.2
  * reflex-process to 0.3.1.0
* New Pins
  * gargoyle-postgresql-connect 0.1.0.0

## v0.6.0.0

* ([#658](https://github.com/reflex-frp/reflex-platform/pull/658)) Include infrastructure for experimental WebGHC support.
* ([#664](https://github.com/reflex-frp/reflex-platform/pull/664)) Update GHCJS to include a critical bugfix to the `-dedupe` flag. (See [obsidiansystems/ghcjs#3](https://github.com/obsidiansystems/ghcjs/pull/3) for more information.)
* Bump
  * `all-cabal-hashes` to more recent snapshot of Hackage.
  * `reflex` to 0.7.1.0.
  * `reflex-dom`/`reflex-dom-core` to 0.6.0.0.
  * `reflex-fsnotify` to 0.2.1.1.
  * `reflex-ghci` to 0.1.4.1.
  * `reflex-process` to 0.3.0.0.
  * `reflex-vty` to 0.1.4.0.
  * `android-activity`. It now detects the number of cores on Android devices to utilize them better.
  * pandoc to 2.11.0.2 (bumping various dependencies as well)
* Remove support for haskell-ide-engine

## v0.5.3.0

* ([#578](https://github.com/reflex-frp/reflex-platform/pull/578)) Bump `patch` to `0.0.3.1`.
* ([#649](https://github.com/reflex-frp/reflex-platform/pull/649), [#655](https://github.com/reflex-frp/reflex-platform/pull/655)) Update `hackGet` to support thunks with `thunk.nix` when packed.

## v0.5.2.0

* Fix issues with iOS deploy script caused by the `openssl` command sometimes resolving to OpenSSL and other times to LibreSSL, which output parsed X.509 certificates in slightly different formats. Now it always uses LibreSSL as provided by Nixpkgs.

* Throw an error in `hackGet` when files other than the `git/github.json` and `default.nix` are there. This is a common problem when `git/github.json` exist in an unpacked thunk.

* Add missing dependencies for Android release builds, fixing the problem with full Android release being broken.

* Allow passing signing parameters to the Android deploy script, instead of Nix build scripts, to match iOS deployments. This can help to avoid leaking secrets into the Nix store, important when the Nix store is publicly readable.

* Fix builds of `servant` in GHCJS.

* Bump `reflex` to `0.6.4.1`.

* Bump `reflex-process` to 0.2.1.0 and `reflex-ghci` to 0.1.4.0.

## v0.5.1.0 - 2020-01-22

* Bump `reflex` to `0.6.4`.

## v0.5.0.0 - 2020-01-21

* Add haskell overlay for `reflex-ghci` and include it in the general dev tools
  set that is available in development shells (e.g., when running `work-on`)

* Make script iOS deploy more flexible.
  Splaces on either side of the `=` printed out by Apple's `security find-certificate` command for the team ID are now accepted.

* Bump `reflex-process` to `0.2.0.0`, a breaking change though small.

* Bump `patch` to `0.0.2.0`.

## v0.4.2.0 - 2020-01-15

* Add haskell overlays for `reflex-process`, `reflex-fsnotify`, and `reflex-vty`

* Bump `jsaddle*` to `0.9.7.0`.

* Test profiling in `release.nix`. Jobs names were shuffled in the process,
  which is acceptible as this file is not intended to be comsumed by anything
  but CI and QA. Add a note explaining that instability.

* Bump `reflex-todomvc`

* Support both `Apple Development` and `iPhone Developer` profiles on iOS deploy.
  If you have many certificates it will take the first one it finds, for that reason it is recommended to put certificates in a user login keychain instead of the system one in multi-user machines.

## v0.4.1.0 - 2020-01-02

* Make Nixpkgs treat GHCJS builds as cross.
  This will fix things like `setup-depends` and `build-tool-depends` being properly built with GHC rather than GHCJS.
  That, in turn, will allow `markdown-unlit` READMEs and other things that we had trouble with.

* Bump cabal-macosx to correctly use the host platform rather than build
  platform to decide whether its appropriate to make a desktop app bundle.

## v0.4.0.0 - 2019-12-31

* Bump nixpkgs to 19.09.
  This will bump the versions of numerous packages, including much of Hackage.

## v0.3.0.0 - 2019-12-30

* Bump ghcjs-dom packages:

   - `ghcjs-dom` to `0.9.4.0`
   - `ghcjs-dom-jsaddle` to `0.9.4.0`
   - `ghcjs-dom-jsffi` to `0.9.4.0`
   - `jsaddle-dom` to `0.9.4.0`

* Bump aeson, and other packages to work with it:

   - `aeson` to `1.4.5.0`
   - `time-compat` to `1.9.2.2`
   - `hpack` to `0.32.0`
   - `webdriver` to `0.9.0.1`

* Bump ghc to include more `8.6` backports

## v0.2.0.0 - 2019-12-30

* Upgrade reflex packages:

   - `reflex` to `0.6.3`
   - `reflex-dom` to `0.5.3`
   - `reflex-dom-core` to `0.5.3`

* Document how to accept android sdk license agreement and pass acceptance
  through to android infrastructure.

* Update to GHC(JS) 8.6.5
  * Apply workaround patch for
    [ghc#16893](https://gitlab.haskell.org/ghc/ghc/issues/16893),
    avoiding segmentation fault when using base.

* Update to the nixos-19.03 nixpkgs channel

* Update to gradle build tools 3.1.0, androidsdk 9, and default to android
  platform version 28

* Bump reflex-dom 0.5.2

* Fixes an inconsistency between nix-shell and nix-build where certain
  Haskell build tools were not being overriden
  ([#548](https://github.com/reflex-frp/reflex-platform/pull/548)

* Removing long-since-broken `nixpkgs.haskell.compiler.ghcSplices` attribute,
  leaving behind `nixpkgs.haskell.compiler.ghcSplices-8_6`, which is its
  intended replacement.

* Add optional `runtimeSharedLibs` parameter to `buildApp`.
  See that function's documentation for how to use it.

* `zlib` for mobile doesn't provide any `*.so`/`.dylib`, that way you are
  guaranteed to link the `.a` and not one of the others by mistake.

* Fix work-on-multi to respect the do-check of the haskell-config derivations

* The following attributes have been deprecated in `default.nix`, and are now
  defined in `nix-utils/hackage.nix`:

   * `attrsToList`
   * `mapSet`
   * `mkSdist`
   * `sdists`
   * `mkHackageDocs`
   * `hackageDocs`
   * `mkReleaseCandidate`
   * `releaseCandidates`

  These are only useful to the maintainers of packages in reflex platform, and
  just clutter the top level for everyone else.

* Deprecate:

   * `generalDevTools`
   * `generalDevToolsAttrs`
   * `nativeHaskellPackages`

  And make `generalDevTools'` as a replacement for just `generalDevToolsAttrs`
  with a more structured argument.

  The are exposed for now for backwards compat, but will be removed at a later
  point. They are mainly used in the implementation of the `work-on*` scripts.

* The following attributes have been deprecated in `default.nix`, and are now
  defined in `nix-utils/work-on-multi.nix`:

   * `workOnMulti`
   * `workOnMulti'`

  If you just used `script/work-on-multi`, nothing has changed.
  But if you do use the nix attributes directly, see how the deprecated exports
  are written to migrate your code to not use them.

## v0.1.0.0 - 2019-04-03

* Move git "thunks" to `dep/` dirs within each overlay, rather than one big
  `dep/` directory at the top level. This does make checking out thunks a bit
  more involved, but has the benefit of untangling reflex-platform a bit and
  making it more modular.

* Many of the overlays have a `_dep` attribute with all the source derivations
  introduced by the overlay. This is just so `release.nix` can build all the
  sources, ensuring that these build-time dependencies also make it to the
  cache. This attribute is *not* intended to be overridden by consumers of
  reflex-platform. In particular it's possible the way we now push to the cache
  doesn't need this, and it will be removed entirely.

* Remove support for GHCs older than 8.4.
